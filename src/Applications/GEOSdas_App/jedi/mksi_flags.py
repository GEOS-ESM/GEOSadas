#!/usr/bin/env python3

import argparse
import glob
import re


def replace_keyword_with_yaml_list(text, keyword, values):
    """
    Replace occurrences like:

        use_flag: &use_channels_gmi_gpm

    with:

        use_flag:
          - 1
          - -1
          - 1

    while preserving YAML indentation.
    """

    pattern = re.compile(
        rf'^(\s*)([A-Za-z0-9_]+)\s*:\s*{re.escape(keyword)}\s*$',
        re.MULTILINE
    )

    def replacement(match):

        base_indent = match.group(1)
        yaml_key = match.group(2)

        list_indent = base_indent + "  "

        lines = [f"{base_indent}{yaml_key}:"]

        for val in values:
            lines.append(f"{list_indent}- {val}")

        return "\n".join(lines)

    return pattern.sub(replacement, text)


def process_file(infile, outfile, keyword, values):

    with open(infile, "r") as f:
        text = f.read()

    updated_text = replace_keyword_with_yaml_list(
        text,
        keyword,
        values
    )

    with open(outfile, "w") as f:
        f.write(updated_text)

    print(f"Processed: {infile} -> {outfile}")


def main():

    parser = argparse.ArgumentParser(
        description="Replace YAML keyword with YAML list values."
    )

    parser.add_argument(
        "--input",
        required=True,
        help="Input YAML file"
    )

    parser.add_argument(
        "--output",
        required=True,
        help="Output YAML file"
    )

    parser.add_argument(
        "--keyword",
        required=True,
        help="Keyword to replace"
    )

    parser.add_argument(
        "--values",
        required=True,
        nargs="+",
        help="Replacement values"
    )

    args = parser.parse_args()

    process_file(
        args.input,
        args.output,
        args.keyword,
        args.values
    )


if __name__ == "__main__":
    main()
