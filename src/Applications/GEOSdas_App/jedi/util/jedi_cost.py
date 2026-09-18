#!/usr/bin/env python
import re
import math
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import matplotlib as mpl
import numpy as np
import argparse
import glob

def get_n_colors(n, name='viridis'):
    """
    Returns a list of n RGBA colors from a specified Matplotlib colormap.

    Parameters:
    n (int): The number of colors to return.
    name (str): The name of the colormap (e.g., 'plasma', 'Dark2', 'Paired').
    
    Returns:
    numpy.ndarray: An array of shape (n, 4) with RGBA float values between 0 and 1.
    """
    if n <= 0:
        return np.array([])
    
    # Get the colormap object by its name
    cmap = mpl.colormaps[name] 
    
    # Take colors at regular intervals spanning the colormap range [0, 1]
    # For a discrete colormap like 'Dark2', simply pass the indices 
    # if n is less than or equal to the map's length (e.g. 8 for Dark2).
    if name in mpl.colormaps.keys() and isinstance(cmap, mpl.colors.ListedColormap) and n <= cmap.N:
        return cmap.colors[:n]
    else:
        return cmap(np.linspace(0, 1, n))

# --- Example Usage ---
# Get 5 colors from the 'plasma' colormap
#colors_plasma = get_n_colors(6, name='plasma')
#print(f"Colors from 'plasma':\n{colors_plasma}\n")

# Get 8 colors from the qualitative 'Paired' colormap
#colors_paired = get_n_colors(8, name='Paired')
#print(f"Colors from 'Paired':\n{colors_paired}\n")

# Plotting example
#fig, ax = plt.subplots(figsize=(6, 3))
#n_lines = 10
#colors = get_n_colors(n_lines, name='hsv') # 'hsv' is good for large n
#
#for i in range(n_lines):
#    ax.plot([0, 1], [i, i], color=colors[i], linewidth=4)
#
#ax.set_title(f'{n_lines} distinct colors from the "hsv" colormap')
#ax.set_yticks(range(n_lines))
#plt.show()


def expand_patterns(patterns):
    if not patterns or isinstance(patterns, str):
        return []
    
    expanded = []
    for pattern in patterns:
        matches = glob.glob(pattern)
        if matches:
            expanded.extend(matches)
        else:
            expanded.append(pattern)
    return expanded

def pcgsoi_cost_and_grad(file_path,linear):
    # Compile regex pattern for matching numbers (including scientific notation)
    number_pattern = re.compile(r"[-+]?\d*\.\d+(?:[eE][-+]?\d+)?|\d+")
    jb_pattern = re.compile(r'J=\s+([+-]?\d+\.\d+E[+-]?\d+)\s+([+-]?\d+\.\d+E[+-]?\d+)\s+([+-]?\d+\.\d+E[+-]?\d+)')

    costs = []
    grads = []
    grads_raw = []
    jb = []; jo = []
    buffer = ""

    with open(file_path, 'r') as file:
        for line in file:
            line = line.strip()

            # Only process lines that start with the expected prefix
            if line.startswith("cost,grad,step"):
                parts = line.split("=")
                if len(parts) != 2:
                    continue  # Skip malformed lines

                values_str = parts[1]
                numbers = number_pattern.findall(values_str)

                if len(numbers) >= 4:
                    cost = 0.5 * float(numbers[2])  # 3rd number
                    grad = float(numbers[3])  # 4th number
                    costs.append(cost)
                    if grad > 0:  # log10 is undefined for 0 or negative values
                       if linear:
                          grads.append(grad)
                       else:
                          grads.append(np.log10(grad))
                    else:
                       grads.append(float('-inf'))  # or handle differently
                    grads_raw.append(grad)

    # Read Jb
    with open(file_path, 'r') as file:
        for line in file:
            match = jb_pattern.search(line)
            if match:
                j = 0.5* float(match.group(1))  # Second value (Jb)
                jb.append(j)

    # derive Jo
    for i, j in enumerate(costs):
        jj = costs[i] - jb[i]
        jo.append(jj)

    # Calculate grad reduction
    reduction = []
    for i, g in enumerate(grads_raw):
        if i == 0:
            if linear:
               reduction.append(1.0)
            else:
               reduction.append(0.0)
        else:
            if grads_raw[i-1] != 0:
                if linear:
                   red = g / grads_raw[0]
                else:
                   red = np.log10(g / grads_raw[0])
                reduction.append(red)
            else:
                reduction.append(float('inf'))  # Avoid divide-by-zero

    return jb, jo, reduction

def get_gsi_cost(filename,noiter0):
    jb = []
    jo = []

    # Regular expression to match the prefix and capture the 7 float numbers
    pattern = re.compile(
        r"truecost::jgrad: grepcost J,Jb,Jo,Jc,Jl,Jd,Jq=\s+"
        r"([-\d.E+]+)\s+([-\d.E+]+)\s+([-\d.E+]+)\s+([-\d.E+]+)\s+([-\d.E+]+)\s+([-\d.E+]+)"
    )

    with open(filename, 'r') as file:
        for line in file:
            match = pattern.search(line)
            if match:
                j = 0.5* float(match.group(2))  # Second value (Jb)
                jb.append(j)
                j = 0.5* float(match.group(3))  # Third value (Jo)
                jo.append(j)

    if noiter0:
       jb.insert(0, math.nan)
       jo.insert(0, math.nan)
    return jb, jo

#-------------------------------
def get_gsi_norm(filename,noiter0,linear):
   grad = []

   with open(filename, 'r') as file:
     for line in file:
         if 'pcglanczos: grepgrad grad,reduction=' in line:
             values = line.split('=')[1].split()
#            int1 = int(values[0])
#            int2 = int(values[1])
#            float1 = float(values[2])
             float2 = float(values[3])
             if float2 > 0:  # log10 is undefined for 0 or negative values
                if linear:
                   grad.append(float2)
                else:
                   grad.append(np.log10(float2))
             else:
                grad.append(float('-inf'))  # or handle differently

   if noiter0:
      grad.insert(0, math.nan)
   return grad

#-------------------------------
def get_jedi_cost(filename):

  # List to store parsed cost values
  jb = []
  jo = []

  # Regular expression to match the line and extract the number after '='
  title = 'Quadratic Cost Function Over Iterations'
  ylab = 'Cost'

  # Read file and extract matching lines
  pattern = re.compile(r"Quadratic cost function:\s+JoJc\(\s*\d+\) = ([\d\.]+)")
  with open(filename, 'r') as file:
    for line in file:
        match = pattern.search(line)
        if match:
            numbers = float(match.group(1))
            jo.append(numbers)

  pattern = re.compile(r"Quadratic cost function:\s+Jb  \(\s*\d+\) = ([\d\.]+)")
  with open(filename, 'r') as file:
    for line in file:
        match = pattern.search(line)
        if match:
            numbers = float(match.group(1))
            jb.append(numbers)

  return jb, jo

#-------------------------------
def get_jedi_norm(filename,linear):

  # List to store parsed cost values
  grad = []

  # Read file and extract matching lines
  with open(filename, "r") as file:
    for line in file:
        # Match lines like "Norm reduction ( 7) = 59.17712266523324"
        match = re.search(r"Norm reduction\s*\(\s*(\d+)\s*\)\s*=\s*([+-]?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)",line)
        if match:
#           index = int(match.group(1))
            value = float(match.group(2))
#           residuals.append((index, value))
#           numbers = float(match.group(0))
            if value > 0:  # log10 is undefined for 0 or negative values
                if linear:
                   grad.append(value)
                else:
                   grad.append(np.log10(value))
            else:
                grad.append(float('-inf'))  # or handle differently

  return grad

#-------------------------------
def show_norm (norm,linear,mycolor,mylabel):

  # Regular expression to match the line and extract the number after '='
  title = 'Norm Reduction Over Iterations'
  if linear:
     ylab = 'Norm Reduction'
  else:
     ylab = 'Log(Norm Reduction)'

  # Plotting the results
# plt.figure(figsize=(10, 5))
  plt.plot(norm, color=mycolor, linestyle='-', label=mylabel)
  plt.title(title)
  plt.xlabel('Iteration')
  plt.ylabel(ylab)
  plt.grid(True)
  plt.tight_layout()
# plt.show()

#-------------------------------
def comp_norm (jedi,gsi,linear):

  # Regular expression to match the line and extract the number after '='
  title = 'Norm Reduction Over Iterations'
  if linear:
     ylab = 'Norm Reduction'
  else:
     ylab = 'Log(Norm Reduction)'

  # Plotting the results
# plt.figure(figsize=(10, 5))
  plt.plot(jedi, color='r', linestyle='-', label='JEDI')
  plt.plot(gsi , color='b', linestyle='-' ,label=' GSI')
  plt.title(title)
  plt.xlabel('Iteration')
  plt.ylabel(ylab)
  plt.grid(True)
  plt.tight_layout()
  plt.legend()
# plt.show()

#-------------------------------
def show_cost(jb,jo,mycolor,mylabel):

  # Regular expression to match the line and extract the number after '='
  title = 'Quadratic Cost Function Over Iterations'
  ylab = 'Cost'

  # Plotting the results
# plt.figure(figsize=(10, 5))
  plt.plot(jb, color=mycolor, linestyle='-', label=mylabel+'-Jb')
  plt.plot(jo, color=mycolor, linestyle='-', label=mylabel+'-Jo')
  plt.title(title)
  plt.xlabel('Iteration')
  plt.ylabel(ylab)
  plt.grid(True)
  plt.tight_layout()
# plt.legend()
# plt.show()

#-------------------------------
def comp_cost(jjb,jjo,gjb,gjo):

  # Regular expression to match the line and extract the number after '='
  title = 'Quadratic Cost Function Over Iterations'
  ylab = 'Cost'

  # Plotting the results
# plt.figure(figsize=(10, 5))
  plt.plot(jjb, color='r', linestyle='-', label='JEDI-Jb')
  plt.plot(jjo, color='r', linestyle='--',label='JEDI-Jo')
  plt.plot(gjb, color='b', linestyle='-' ,label=' GSI-Jb')
  plt.plot(gjo, color='b', linestyle='--',label=' GSI-Jo')
  plt.title(title)
  plt.xlabel('Iteration')
  plt.ylabel(ylab)
  plt.grid(True)
  plt.tight_layout()
  plt.legend()
# plt.show()

#-------------------------------

# Set up command-line argument parsing
parser = argparse.ArgumentParser(description='Plot minimization diagnostics.')
parser.add_argument('--jedi', nargs='+', type=str, default='null',
                      help='JEDI log file (default: none)')
parser.add_argument('--gsi', nargs='+', type=str, default='null',
                      help='GSI log file (default: none)')
parser.add_argument('--norm', action='store_true',
                        help='Plot norm reduction (default: False)')
parser.add_argument('--pcgsoi', action='store_true',
                        help='Cost and norm from PCGSOI (default: False)')
parser.add_argument('--noiter0', action='store_true',
                        help='GSI has no iter0 (default: False)')
parser.add_argument('--linear', action='store_true',
                        help='Do not take log (default: False)')
parser.add_argument('--label', nargs='+', type=str, default='null',
                      help='JEDI log file (default: none)')
parser.add_argument('--color', nargs='+', type=str, default='null',
                      help='JEDI log file (default: none)')
parser.add_argument('--fig', type=str, default='none',
                      help='give a filename to save plot (default: none)')
args = parser.parse_args()


got_gsi=False; gnorm = False; gcost = False
got_jedi=False; jnorm = False; jcost = False
if args.gsi:
   gsi_files = expand_patterns(args.gsi) if args.gsi else []
   if gsi_files != []:
      got_gsi = True
   
if args.jedi:
   jedi_files = expand_patterns(args.jedi) if args.jedi else []
   if jedi_files != []:
      got_jedi = True

# start page of plot
plt.figure(figsize=(10, 5))

# if so, read GSI data 
if got_gsi and not got_jedi:
  n = len(gsi_files)
  if  args.color == 'null':
    colors = get_n_colors(len(gsi_files), name='hsv')
  else:
    colors = args.color
  if args.label == "null":
     labels = [chr(97 + i) for i in range(n)]
  else:
     labels = args.label
  i=0
  for file in gsi_files:
     # if so, get cost and gradient from pcgsoi
     if args.pcgsoi:
        jb,jo,grad = pcgsoi_cost_and_grad(file,args.linear)
        if args.norm:
           show_norm(grad,args.linear,colors[i],labels[i])
        else:
           show_cost(jb,jo,colors[i],labels[i])
     else:
        if args.norm:
           ggnorm = get_gsi_norm(file,args.noiter0,args.linear)
           gnorm = True
           show_norm(ggnorm,args.linear,colors[i],labels[i])
        else:
           gjb, gjo = get_gsi_cost(file,args.noiter0)
           gcost = True
           show_cost(gjb,gjo,colors[i],labels[i])
     i = i + 1
  plt.legend()

# if so, read JEDI data 
if got_jedi and not got_gsi:
  n = len(jedi_files)
  if args.color == "null":
     colors = get_n_colors(n, name='hsv')
  else:
     colors = args.color
  if args.label == "null":
     labels = [chr(97 + i) for i in range(n)]
  else:
     labels = args.label
  i=0
  for file in jedi_files:
     if args.norm:
        jjnorm = get_jedi_norm(file,args.linear)
        jnorm = True
        if not got_gsi:
           show_norm(jjnorm,args.linear,colors[i],labels[i])
     else:
        jjb, jjo = get_jedi_cost(file)
        jcost = True
        if not got_gsi:
           show_cost(jjb,jjo,colors[i],labels[i])
     i = i + 1
  plt.legend()

# when two cases are passed
if got_jedi and got_gsi:
  for gfile,jfile in zip(gsi_files, jedi_files):
     if args.norm:
        jjnorm = get_jedi_norm(jfile,args.linear)
        ggnorm = get_gsi_norm (gfile,args.noiter0,args.linear)
        comp_norm(jjnorm,ggnorm,args.linear)
     else:
        jjb, jjo = get_jedi_cost(jfile)
        gjb, gjo = get_gsi_cost (gfile,args.noiter0)
        comp_cost(jjb,jjo,gjb,gjo)

if args.fig == 'none':
  plt.show()
else:
  plt.savefig(args.fig,dpi=300,orientation='landscape',format='png')
