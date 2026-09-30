#!/usr/bin/env python3
"""Derive two GNSS-RO PBL-height products from grouped IODA NetCDF files.

Faithful Python translation of
``read_gnssro_twopeaks_wsurfelev_qc_compact.ncl``.

Dependencies: numpy, netCDF4

The algorithm:
  * separates flat ROSEQ3 observations into profiles using dateTime changes;
  * maps latitude/longitude from a companion bending-angle metadata file;
  * removes duplicate altitude levels (1-mm tolerance);
  * derives refractivity gradients and up to two PBL peaks;
  * applies surface-elevation and negative-PBLH profile QC; and
  * writes compact first- and second-PBLH NetCDF products.

Unlike the original NCL file, paths and processing time are command-line
arguments. Defaults reproduce its 2024-09-03 00 UTC settings.
"""

from __future__ import annotations

import argparse
import sys
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

import numpy as np
from netCDF4 import Dataset, num2date


FLOAT_FILL = np.float32(-9.9e10)
INT_FILL = np.int32(-999)
ALT_TOL = 0.001  # m


@dataclass(frozen=True)
class Config:
    infile: Path
    metafile: Path
    sfcfile: Path
    first_output: Path
    second_output: Path
    cycle_hour: int
    threshold: float = -40.0
    zmax_pbl_base: float = 6000.0
    peak_sep: float = 500.0


def parse_args() -> Config:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--date", default="20240903", help="Cycle date YYYYMMDD")
    p.add_argument("--hour", default="00", help="Cycle hour HH")
    p.add_argument("--input-dir", type=Path,
                   default=Path("/discover/nobackup/mganesha/DSI_PBL/fromWei/output"))
    p.add_argument("--infile", type=Path, help="Refractivity-only IODA file")
    p.add_argument("--metafile", type=Path, help="Metadata/bending-angle IODA file")
    p.add_argument("--sfcfile", type=Path, default=Path(
        "/gpfsm/dnb10/projects/p14/pub/f5430_fp/das/"
        "GEOS.fp.asm.const_2d_asm_Nx.00000000_0000.V01.nc4"))
    p.add_argument("--first-output-dir", type=Path,
                   default=Path("./output/twopeaks/thresh40_FPsfcelev_qc/python/firstPBLH"))
    p.add_argument("--second-output-dir", type=Path,
                   default=Path("./output/twopeaks/thresh40_FPsfcelev_qc/python/secondPBLH"))
    p.add_argument("--threshold", type=float, default=-40.0)
    p.add_argument("--zmax-pbl-base", type=float, default=6000.0)
    p.add_argument("--peak-separation", type=float, default=500.0)
    a = p.parse_args()

    try:
        datetime.strptime(a.date + a.hour, "%Y%m%d%H")
    except ValueError as exc:
        p.error(str(exc))
    stem = a.date + a.hour
    infile = a.infile or a.input_dir / f"gnssro_obs_{stem}.nc4"
    metafile = a.metafile or a.input_dir / f"gnssro_metadata_bending_angle_{stem}.nc4"
    filename = f"gnssro_pbl_obs_{stem}.nc4"
    return Config(
        infile=infile, metafile=metafile, sfcfile=a.sfcfile,
        first_output=a.first_output_dir / filename,
        second_output=a.second_output_dir / filename,
        cycle_hour=int(a.hour), threshold=a.threshold,
        zmax_pbl_base=a.zmax_pbl_base, peak_sep=a.peak_separation,
    )


def require_file(path: Path, description: str) -> None:
    if not path.is_file():
        raise FileNotFoundError(f"{description} not found: {path}")


def read_masked(var: Any, dtype: np.dtype | type | None = None) -> np.ma.MaskedArray:
    """Read a NetCDF variable as a masked array without applying scale twice."""
    data = np.ma.asarray(var[:])
    if dtype is not None:
        data = data.astype(dtype)
    return data


def group_var(ds: Dataset, group: str, name: str) -> Any:
    try:
        return ds.groups[group].variables[name]
    except KeyError as exc:
        raise KeyError(f"Required variable {group}/{name} is absent from {ds.filepath()}") from exc


def profile_ends_from_time_or_height(date_sec: np.ndarray,
                                     altitude: np.ma.MaskedArray) -> np.ndarray:
    if date_sec.size < 2:
        raise ValueError("At least two locations are required")
    ends = np.flatnonzero(date_sec[1:] != date_sec[:-1])
    if ends.size == 0:
        print("WARNING: dateTime did not change; falling back to height decreases")
        delta = np.ma.asarray(altitude[1:] - altitude[:-1])
        ends = np.flatnonzero(np.ma.filled(delta < 0.0, False))
    if ends.size == 0:
        raise ValueError("Could not determine profile boundaries from dateTime or height")
    return np.r_[ends, date_sec.size - 1].astype(np.int64)


def profile_ends_from_time(date_sec: np.ndarray) -> np.ndarray:
    ends = np.flatnonzero(date_sec[1:] != date_sec[:-1])
    return (np.array([date_sec.size - 1], dtype=np.int64) if ends.size == 0
            else np.r_[ends, date_sec.size - 1].astype(np.int64))


def first_valid_profile_coordinates(lat: np.ma.MaskedArray,
                                    lon: np.ma.MaskedArray,
                                    date_sec: np.ndarray) -> tuple[np.ma.MaskedArray, np.ma.MaskedArray, bool]:
    """Return profile coordinates and whether input coordinates are 2-D."""
    if lat.ndim == 2 and lon.ndim == 2:
        if lat.shape != lon.shape:
            raise ValueError(f"2-D latitude/longitude shapes differ: {lat.shape}, {lon.shape}")
        nprof = lat.shape[0]
        lat_prof = np.ma.masked_all(nprof, dtype=np.float32)
        lon_prof = np.ma.masked_all(nprof, dtype=np.float32)
        for i in range(nprof):
            good = np.flatnonzero(~np.ma.getmaskarray(lat[i]) & ~np.ma.getmaskarray(lon[i]))
            if good.size:
                lat_prof[i], lon_prof[i] = lat[i, good[0]], lon[i, good[0]]
        return lat_prof, lon_prof, True
    if lat.ndim != 1 or lon.ndim != 1 or lat.shape != lon.shape:
        raise ValueError("Metadata latitude and longitude must be matching rank-1 or rank-2 arrays")
    if date_sec.size != lat.size:
        raise ValueError("Metadata dateTime and rank-1 latitude/longitude lengths differ")
    ends = profile_ends_from_time(date_sec)
    lat_prof = np.ma.masked_all(ends.size, dtype=np.float32)
    lon_prof = np.ma.masked_all(ends.size, dtype=np.float32)
    start = 0
    for i, end in enumerate(ends):
        sl = slice(start, end + 1)
        good = np.flatnonzero(~np.ma.getmaskarray(lat[sl]) & ~np.ma.getmaskarray(lon[sl]))
        if good.size:
            lat_prof[i], lon_prof[i] = lat[start + good[0]], lon[start + good[0]]
        elif start < lat.size:
            lat_prof[i], lon_prof[i] = lat[start], lon[start]
        start = int(end) + 1
    return lat_prof, lon_prof, False


def epoch_datetimes(date_sec: np.ndarray) -> list[Any]:
    return list(num2date(date_sec.astype(np.float64),
                         "seconds since 1970-01-01 00:00:00",
                         calendar="standard", only_use_cftime_datetimes=True))


def strings_to_char(strings: list[str], width: int) -> np.ndarray:
    """NCL tochar(string-array) equivalent: fixed-width S1 character array."""
    raw = np.asarray([s.encode("ascii") for s in strings], dtype=f"S{width}")
    return raw.view("S1").reshape(len(strings), width)


def map_level_coordinates(target_count: int, source_count: int) -> np.ndarray:
    if target_count <= 1:
        return np.zeros(target_count, dtype=np.int64)
    # NCL tointeger(x + 0.5) for nonnegative x is round-half-up.
    return np.floor(np.arange(target_count) * (source_count - 1) /
                    (target_count - 1) + 0.5).astype(np.int64)


def unique_valid_levels(alt: np.ma.MaskedArray, ref: np.ma.MaskedArray,
                        lat: np.ma.MaskedArray, lon: np.ma.MaskedArray,
                        timeoff: np.ma.MaskedArray,
                        dtchars: np.ndarray) -> tuple[np.ndarray, ...]:
    """Retain first occurrence of each valid altitude, preserving level order."""
    keep: list[int] = []
    for j in range(alt.size):
        if np.ma.is_masked(alt[j]) or np.ma.is_masked(ref[j]):
            continue
        if not any(abs(float(alt[j]) - float(alt[k])) <= ALT_TOL for k in keep):
            keep.append(j)
    idx = np.asarray(keep, dtype=np.int64)
    return (np.asarray(alt[idx]), np.asarray(ref[idx]), np.asarray(lat[idx]),
            np.asarray(lon[idx]), np.asarray(timeoff[idx]), dtchars[idx])


def interpolate_missing_1d(values: np.ma.MaskedArray) -> np.ndarray:
    """Equivalent needed here for NCL linmsg(...,-1): linear interior filling."""
    y = np.ma.asarray(values, dtype=np.float64)
    mask = np.ma.getmaskarray(y) | ~np.isfinite(np.ma.filled(y, np.nan))
    if not mask.any():
        return np.asarray(y)
    x = np.arange(y.size)
    good = ~mask
    if not good.any():
        return np.full(y.size, np.nan)
    out = np.asarray(np.ma.filled(y, np.nan), dtype=np.float64)
    interior = mask & (x >= x[good][0]) & (x <= x[good][-1])
    out[interior] = np.interp(x[interior], x[good], out[good])
    return out


def local_minima(values: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    """Interior strict/local plateau minima, mirroring NCL local_min_1d use.

    Endpoints are intentionally excluded; the source explicitly handles a
    global minimum at the first gradient level as a non-local minimum.
    """
    indices: list[int] = []
    n = values.size
    i = 1
    while i < n - 1:
        if not np.isfinite(values[i]):
            i += 1
            continue
        left = i
        right = i
        while right + 1 < n and values[right + 1] == values[i]:
            right += 1
        if (left > 0 and right < n - 1 and np.isfinite(values[left - 1])
                and np.isfinite(values[right + 1])
                and values[i] < values[left - 1] and values[i] < values[right + 1]):
            indices.append(left)
        i = right + 1
    idx = np.asarray(indices, dtype=np.int64)
    return values[idx], idx


def nearest_index(value: float, coordinates: np.ma.MaskedArray) -> int | None:
    arr = np.ma.asarray(coordinates, dtype=np.float64)
    valid = ~np.ma.getmaskarray(arr) & np.isfinite(np.ma.filled(arr, np.nan))
    if not valid.any():
        return None
    candidates = np.flatnonzero(valid)
    return int(candidates[np.argmin(np.abs(np.asarray(arr)[valid] - value))])


def create_variable(ds: Dataset, name: str, dtype: np.dtype | str,
                    dimensions: tuple[str, ...], fill: Any | None = None,
                    attrs: dict[str, str] | None = None) -> Any:
    kwargs = {} if fill is None else {"fill_value": fill}
    var = ds.createVariable(name, dtype, dimensions, **kwargs)
    if attrs:
        var.setncatts(attrs)
    return var


def write_output(path: Path, source_files: tuple[Path, Path], arrays: dict[str, np.ndarray],
                 second: bool) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if path.exists():
        path.unlink()
    nprof, klev = arrays["alt"].shape
    ndate = arrays["time_start"].shape[1]
    ndatelocal = arrays["localtime_start"].shape[1]
    with Dataset(path, "w", format="NETCDF4") as out:
        out.setncatts({
            "title": "PBL height derived from GNSS-RO",
            "source_refractivity_file": str(source_files[0]),
            "source_metadata_file": str(source_files[1]),
            "Conventions": "None",
            "creation_date": datetime.now().astimezone().strftime("%a %b %d %H:%M:%S %Z %Y"),
        })
        for name, size in (("profiles", nprof), ("levels", klev),
                           ("ndate", ndate), ("ndatelocal", ndatelocal)):
            out.createDimension(name, size)
        specs = {
            "time_start": ("S1", ("profiles", "ndate"), None,
                           {"units": "yyyy-mm-dd_hh:mi:ss", "long_name": "UTC time at first level"}),
            "localtime_start": ("S1", ("profiles", "ndatelocal"), None,
                                {"units": "yyyy-mm-dd_hh:mi", "long_name": "Local time at first level"}),
            "lat": ("f4", ("profiles", "levels"), FLOAT_FILL,
                    {"long_name": "Latitude profile", "units": "degrees_north"}),
            "lon": ("f4", ("profiles", "levels"), FLOAT_FILL,
                    {"long_name": "Longitude profile", "units": "degrees_east"}),
            "ref": ("f4", ("profiles", "levels"), FLOAT_FILL,
                    {"long_name": "Atmospheric refractivity", "units": "N"}),
            "alt": ("f4", ("profiles", "levels"), FLOAT_FILL,
                    {"long_name": "Geopotential height", "units": "Meters"}),
            "ref_gradient": ("f4", ("profiles", "levels"), FLOAT_FILL,
                             {"long_name": "Refractivity Gradient", "units": "N-unit/km"}),
            "PBLH": ("f4", ("profiles",), FLOAT_FILL,
                     {"long_name": ("Higher" if second else "Lower") + " Planetary Boundary Layer height",
                      "units": "Meters"}),
            "latitude": ("f4", ("profiles",), FLOAT_FILL,
                         {"long_name": "Latitude at PBL level", "units": "degrees_north"}),
            "longitude": ("f4", ("profiles",), FLOAT_FILL,
                          {"long_name": "Longitude at PBL level", "units": "degrees_east"}),
            "latstart": ("f4", ("profiles",), FLOAT_FILL,
                         {"long_name": "Latitude at first level", "units": "degrees_north"}),
            "lonstart": ("f4", ("profiles",), FLOAT_FILL,
                         {"long_name": "Longitude at first level", "units": "degrees_east"}),
            "sfc_elev": ("f4", ("profiles",), FLOAT_FILL,
                         {"long_name": "MERRA-2 surface elevation at nearest grid point",
                          "units": "Meters", "source": "PHIS/9.8"}),
            "alt_start": ("f4", ("profiles",), FLOAT_FILL,
                          {"long_name": "Altitude at first retained RO level", "units": "Meters"}),
            "time_pblh": ("S1", ("profiles", "ndate"), None,
                          {"units": "yyyy-mm-dd_hh:mi:ss", "long_name": "UTC time at PBL level"}),
            "QC": ("i4", ("profiles",), INT_FILL,
                   {"long_name": "PBLH peak QC flag: 0=same peak, 1=two distinct peaks"}),
        }
        # Preserve input satellite-ID dtypes, as NCL typeof() does.
        for sat_name in ("gnss_satid", "ref_satid", "occ_satid"):
            specs[sat_name] = (arrays[sat_name].dtype, ("profiles",), None, {})
        for name, (dtype, dims, fill, attrs) in specs.items():
            var = create_variable(out, name, dtype, dims, fill, attrs)
            var[:] = arrays[name]


def process(cfg: Config) -> None:
    for path, label in ((cfg.infile, "Refractivity input file"),
                        (cfg.metafile, "Metadata/bending-angle input file"),
                        (cfg.sfcfile, "Surface-elevation file")):
        require_file(path, label)

    with Dataset(cfg.infile) as f, Dataset(cfg.metafile) as fm, Dataset(cfg.sfcfile) as fsfc:
        ref = read_masked(group_var(f, "ObsValue", "atmosphericRefractivity"), np.float32)
        alt = read_masked(group_var(f, "MetaData", "height"), np.float32)
        date_sec = np.asarray(group_var(f, "MetaData", "dateTime")[:], dtype=np.int64)
        lat_meta = read_masked(group_var(fm, "MetaData", "latitude"), np.float32)
        lon_meta = read_masked(group_var(fm, "MetaData", "longitude"), np.float32)
        date_sec_meta = np.asarray(group_var(fm, "MetaData", "dateTime")[:], dtype=np.int64)

        # The NCL source reads these from the refractivity file's MetaData group.
        gnss_ids = read_masked(group_var(f, "MetaData", "satelliteConstellationRO"))
        ref_ids = read_masked(group_var(f, "MetaData", "satelliteTransmitterId"))
        occ_ids = read_masked(group_var(f, "MetaData", "satelliteIdentifier"))

        merra_lat = read_masked(fsfc.variables["lat"], np.float64)
        merra_lon = read_masked(fsfc.variables["lon"], np.float64)
        phis = read_masked(fsfc.variables["PHIS"], np.float32)
        sfc_elev = (phis[0] if phis.ndim == 3 else phis) / np.float32(9.8)

    if ref.ndim != 1 or alt.ndim != 1 or ref.size != alt.size or date_sec.size != ref.size:
        raise ValueError("Refractivity, height, and dateTime must be equal-length 1-D arrays")
    if ref.count() == 0 or alt.count() == 0:
        raise ValueError("Refractivity or height is completely missing")
    nrecs = ref.size
    print(f"Total locations = {nrecs}")
    print(f"Valid refractivity count = {ref.count()}")
    print(f"Valid height count = {alt.count()}")

    lat_prof, lon_prof, metadata_2d = first_valid_profile_coordinates(
        lat_meta, lon_meta, date_sec_meta)
    nmeta_prof = lat_prof.size
    ends = profile_ends_from_time_or_height(date_sec, alt)
    starts = np.r_[0, ends[:-1] + 1]
    counts = ends - starts + 1
    nprof = ends.size
    raw_klev = int(counts.max())
    print(f"Total profiles = {nprof}; raw levels/profile min={counts.min()}, max={raw_klev}")
    if any(x.size < nrecs for x in (gnss_ids, ref_ids, occ_ids)):
        raise ValueError("Satellite-ID arrays are shorter than the refractivity locations")

    dts = epoch_datetimes(date_sec)
    utc_hours_all = np.asarray([d.hour + d.minute / 60.0 + d.second / 3600.0 for d in dts])
    dt_strings = [f"{d.year:04d}-{d.month:02d}-{d.day:02d}_{d.hour:02d}:{d.minute:02d}:{int(d.second):02d}" for d in dts]
    dt_chars = strings_to_char(dt_strings, 19)

    profiles: list[dict[str, Any]] = []
    duplicates = 0
    low_unique = 0
    for i, (start, end, count) in enumerate(zip(starts, ends, counts)):
        sl = slice(int(start), int(end) + 1)
        plat = np.ma.masked_all(int(count), dtype=np.float32)
        plon = np.ma.masked_all(int(count), dtype=np.float32)
        if i < nmeta_prof:
            if metadata_2d:
                src_nlev = lat_meta.shape[1]
                mi = (np.arange(int(count)) if count == src_nlev
                      else map_level_coordinates(int(count), src_nlev))
                plat, plon = lat_meta[i, mi], lon_meta[i, mi]
            else:
                plat[:] = lat_prof[i]
                plon[:] = lon_prof[i]
        t_off = np.ma.asarray(utc_hours_all[sl] - cfg.cycle_hour)
        vals = unique_valid_levels(alt[sl], ref[sl], plat, plon, t_off, dt_chars[sl])
        duplicates += int(count) - vals[0].size
        if vals[0].size < 4:
            low_unique += 1
            vals = tuple(v[:0] for v in vals)
        profiles.append({"alt": vals[0], "ref": vals[1], "lat": vals[2], "lon": vals[3],
                         "timeoff": vals[4], "dt": vals[5], "start": int(start)})
    klev = max(p["alt"].size for p in profiles)
    if klev <= 1:
        raise ValueError("Need at least two unique altitude levels in at least one profile")
    print(f"Duplicate altitude levels removed = {duplicates}")
    print(f"Profiles with fewer than 4 unique levels set to missing = {low_unique}")

    def matrix(key: str) -> np.ma.MaskedArray:
        out = np.ma.masked_all((nprof, klev), dtype=np.float32)
        for ii, p in enumerate(profiles):
            out[ii, :p[key].size] = p[key]
        return out

    altitude, refractivity = matrix("alt"), matrix("ref")
    latitude, longitude = matrix("lat"), matrix("lon")
    nlevels = np.asarray([p["alt"].size for p in profiles], dtype=np.int64)
    lat_start, lon_start = latitude[:, 0].copy(), longitude[:, 0].copy()
    alt_start = altitude[:, 0].copy()

    gnss_sat = np.ma.asarray(gnss_ids[starts])
    ref_sat = np.ma.asarray(ref_ids[starts])
    occ_sat = np.ma.asarray(occ_ids[starts])
    utc_hour = utc_hours_all[starts]
    utc_time = strings_to_char([dt_strings[s] for s in starts], 19)

    sfc_prof = np.ma.masked_all(nprof, dtype=np.float32)
    for i in range(nprof):
        if np.ma.is_masked(lat_start[i]) or np.ma.is_masked(lon_start[i]):
            continue
        lon_value = float(lon_start[i])
        if lon_value > 180.0:
            lon_value -= 360.0
        ilat, ilon = nearest_index(float(lat_start[i]), merra_lat), nearest_index(lon_value, merra_lon)
        if ilat is not None and ilon is not None:
            sfc_prof[i] = sfc_elev[ilat, ilon]

    gradient = np.ma.masked_all((nprof, klev), dtype=np.float32)
    for i, nlev in enumerate(nlevels):
        if nlev > 1:
            dz_km = np.diff(np.asarray(altitude[i, :nlev], dtype=np.float64)) * 0.001
            dr = np.diff(np.asarray(refractivity[i, :nlev], dtype=np.float64))
            with np.errstate(divide="ignore", invalid="ignore"):
                g = dr / dz_km
            gradient[i, :nlev - 1] = np.ma.masked_invalid(g.astype(np.float32))

    first_g = np.ma.masked_all(nprof, dtype=np.float32)
    first_h = np.ma.masked_all(nprof, dtype=np.float32)
    second_g = np.ma.masked_all(nprof, dtype=np.float32)
    second_h = np.ma.masked_all(nprof, dtype=np.float32)
    first_lat = np.ma.masked_all(nprof, dtype=np.float32)
    first_lon = np.ma.masked_all(nprof, dtype=np.float32)
    second_lat = np.ma.masked_all(nprof, dtype=np.float32)
    second_lon = np.ma.masked_all(nprof, dtype=np.float32)
    first_time = np.full((nprof, 19), b"", dtype="S1")
    second_time = np.full((nprof, 19), b"", dtype="S1")
    valid_pbl = np.zeros(nprof, dtype=bool)
    counters = dict(cnt1=0, cnt2=0, cnt3=0, cnt4=0, cnt5=0,
                    cnt6=0, cnt7=0, cnt8=0, cnt9=0, cnt10=0)

    def assign_peak(i: int, idx: int, second: bool = False) -> None:
        target = (second_g, second_h, second_lat, second_lon, second_time) if second else (
            first_g, first_h, first_lat, first_lon, first_time)
        target[0][i], target[1][i] = gradient[i, idx], altitude[i, idx]
        target[2][i], target[3][i], target[4][i] = latitude[i, idx], longitude[i, idx], profiles[i]["dt"][idx]

    for i, nlev in enumerate(nlevels):
        if nlev < 4:
            continue
        valid_g_idx = np.flatnonzero(~np.ma.getmaskarray(gradient[i]))
        if valid_g_idx.size == 0:
            counters["cnt10"] += 1
            continue
        min_idx = int(valid_g_idx[np.argmin(np.asarray(gradient[i, valid_g_idx]))])
        counters["cnt1"] += 1
        assign_peak(i, min_idx)
        if float(first_g[i]) >= cfg.threshold:
            first_g[i] = first_h[i] = first_lat[i] = first_lon[i] = np.ma.masked
            first_time[i] = b""
        second_g[i], second_h[i] = first_g[i], first_h[i]
        second_lat[i], second_lon[i], second_time[i] = first_lat[i], first_lon[i], first_time[i]

        zmax = cfg.zmax_pbl_base + (0.0 if np.ma.is_masked(sfc_prof[i]) else float(sfc_prof[i]))
        max_ht = int(np.argmin(np.abs(np.asarray(altitude[i, :nlev]) - zmax)))
        max_ht = min(max_ht, int(nlev) - 2)
        if max_ht <= 0 or np.ma.is_masked(first_g[i]):
            counters["cnt3"] += 1
            first_g[i] = first_h[i] = first_lat[i] = first_lon[i] = np.ma.masked
            second_g[i] = second_h[i] = second_lat[i] = second_lon[i] = np.ma.masked
            first_time[i] = second_time[i] = b""
            continue
        dz_km = np.diff(np.asarray(altitude[i, :nlev], dtype=np.float64)) * 0.001
        gg = np.asarray(gradient[i, :nlev - 1], dtype=np.float64)
        denom = np.sum(dz_km[:max_ht + 1])
        ref_avg = np.sum(dz_km[:max_ht + 1] * gg[:max_ht + 1]) / denom if denom != 0 else np.nan
        delta = 0.25 * float(first_g[i]) + 0.75 * ref_avg
        if not np.isfinite(delta):
            counters["cnt3"] += 1
            first_g[i] = first_h[i] = first_lat[i] = first_lon[i] = np.ma.masked
            second_g[i] = second_h[i] = second_lat[i] = second_lon[i] = np.ma.masked
            first_time[i] = second_time[i] = b""
            continue
        counters["cnt2"] += 1
        valid_pbl[i] = True
        filled = interpolate_missing_1d(gradient[i, :max_ht + 1])
        qmin, imin = local_minima(filled)
        order = np.argsort(qmin, kind="stable")
        qmin, imin = qmin[order], imin[order]
        if imin.size:
            counters["cnt4"] += 1
            if imin[0] == min_idx:
                counters["cnt6"] += 1
                j0 = 1
            else:
                counters["cnt7"] += 1
                assign_peak(i, min_idx)
                j0 = 0
            for j in range(j0, qmin.size):
                if abs(float(altitude[i, imin[j]]) - float(first_h[i])) > cfg.peak_sep and qmin[j] < delta:
                    counters["cnt8" if imin[0] == min_idx else "cnt9"] += 1
                    assign_peak(i, int(imin[j]), second=True)
                    break
        else:
            counters["cnt5"] += 1
            assign_peak(i, min_idx, second=True)

    # A second peak failing the fixed threshold collapses to the first peak.
    collapse = (~np.ma.getmaskarray(second_g)) & (second_g >= cfg.threshold)
    second_g[collapse], second_h[collapse] = first_g[collapse], first_h[collapse]
    second_lat[collapse], second_lon[collapse] = first_lat[collapse], first_lon[collapse]
    second_time[collapse] = first_time[collapse]

    lower_first = np.ma.filled(first_h <= second_h, False)
    pbl_g = np.ma.where(lower_first, first_g, second_g)
    pbl_h = np.ma.where(lower_first, first_h, second_h)
    pbl_lat = np.ma.where(lower_first, first_lat, second_lat)
    pbl_lon = np.ma.where(lower_first, first_lon, second_lon)
    higher_first = np.ma.filled(first_h >= second_h, False)
    pbl_g2 = np.ma.where(higher_first, first_g, second_g)
    pbl_h2 = np.ma.where(higher_first, first_h, second_h)
    pbl_lat2 = np.ma.where(higher_first, first_lat, second_lat)
    pbl_lon2 = np.ma.where(higher_first, first_lon, second_lon)
    for arr, g in ((pbl_h, pbl_g), (pbl_lat, pbl_g), (pbl_lon, pbl_g),
                   (pbl_h2, pbl_g2), (pbl_lat2, pbl_g2), (pbl_lon2, pbl_g2)):
        arr.mask = np.ma.getmaskarray(arr) | np.ma.getmaskarray(g) | (np.ma.filled(g, np.inf) >= cfg.threshold) | ~valid_pbl

    reject_alt = (~np.ma.getmaskarray(alt_start) & ~np.ma.getmaskarray(sfc_prof)
                  & (alt_start < sfc_prof))
    reject_neg = ~np.ma.getmaskarray(pbl_h) & (pbl_h < 0.0)
    reject = np.asarray(reject_alt | reject_neg, dtype=bool)
    keep = np.flatnonzero(~reject)
    if keep.size == 0:
        raise ValueError("All profiles were rejected by profile-level QC")
    print(f"Profiles rejected: alt_start below surface = {reject_alt.sum()}")
    print(f"Profiles rejected: negative firstPBLH = {reject_neg.sum()}")
    print(f"Profiles retained for output = {keep.size} of {nprof}")

    lower_time = np.full_like(first_time, b"")
    higher_time = np.full_like(second_time, b"")
    for i in range(nprof):
        if np.ma.is_masked(pbl_h2[i]):
            continue
        if not np.ma.is_masked(first_h[i]) and not np.ma.is_masked(second_h[i]) and first_h[i] > second_h[i]:
            lower_time[i], higher_time[i] = second_time[i], first_time[i]
        else:
            lower_time[i], higher_time[i] = first_time[i], second_time[i]

    local_hours = np.mod(np.ma.asarray(lon_start) / 15.0 + utc_hour, 24.0)
    local_strings: list[str] = []
    for i, start in enumerate(starts):
        d = dts[start]
        if np.ma.is_masked(local_hours[i]) or not np.isfinite(float(local_hours[i])):
            # NCL would warn and produce fill-like integers; use a deterministic blank string.
            local_strings.append(" " * 16)
            continue
        hour = float(local_hours[i])
        minute = int(np.rint((hour - np.floor(hour)) * 60.0))
        hh = int(np.ceil(hour)) if minute == 60 else int(np.floor(hour))
        minute = 0 if minute == 60 else minute
        local_strings.append(f"{d.year:04d}-{d.month:02d}-{d.day:02d}_{hh % 24:02d}:{minute:02d}")
    local_time = strings_to_char(local_strings, 16)

    qc = np.zeros(nprof, dtype=np.int32)
    distinct = (~np.ma.getmaskarray(pbl_h) & ~np.ma.getmaskarray(pbl_h2)
                & (np.asarray(pbl_h) != np.asarray(pbl_h2)))
    qc[distinct] = 1

    def filled(a: Any, fill: Any = FLOAT_FILL) -> np.ndarray:
        return np.ma.filled(np.ma.asarray(a), fill)

    common = {
        "time_start": utc_time[keep], "localtime_start": local_time[keep],
        "lat": filled(latitude[keep]), "lon": filled(longitude[keep]),
        "ref": filled(refractivity[keep]), "alt": filled(altitude[keep]),
        "ref_gradient": filled(gradient[keep]),
        "gnss_satid": filled(gnss_sat[keep], getattr(gnss_sat, "fill_value", 0)),
        "ref_satid": filled(ref_sat[keep], getattr(ref_sat, "fill_value", 0)),
        "occ_satid": filled(occ_sat[keep], getattr(occ_sat, "fill_value", 0)),
        "latstart": filled(lat_start[keep]), "lonstart": filled(lon_start[keep]),
        "sfc_elev": filled(sfc_prof[keep]), "alt_start": filled(alt_start[keep]),
        "QC": qc[keep],
    }
    first_arrays = common | {"PBLH": filled(pbl_h[keep]), "latitude": filled(pbl_lat[keep]),
                             "longitude": filled(pbl_lon[keep]), "time_pblh": lower_time[keep]}
    second_arrays = common | {"PBLH": filled(pbl_h2[keep]), "latitude": filled(pbl_lat2[keep]),
                              "longitude": filled(pbl_lon2[keep]), "time_pblh": higher_time[keep]}
    write_output(cfg.first_output, (cfg.infile, cfg.metafile), first_arrays, second=False)
    write_output(cfg.second_output, (cfg.infile, cfg.metafile), second_arrays, second=True)
    print(f"Non-missing first PBLH = {pbl_h[keep].count()}")
    print(f"Non-missing second PBLH = {pbl_h2[keep].count()}")
    print(f"Wrote: {cfg.first_output}")
    print(f"Wrote: {cfg.second_output}")


def main() -> int:
    try:
        process(parse_args())
    except (FileNotFoundError, KeyError, ValueError, OSError) as exc:
        print(f"FATAL: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
