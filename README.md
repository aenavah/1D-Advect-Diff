# Linear Advection-Diffusion Equation

## Directory

```bash
../Project/Code
```

## Inputs

Example "input.txt":

##### Diffusion

```
type 1
Nx 32
adv_vl 1.0
k 1.156
x_a 0.0
x_b 1.0
C 1.0
```

##### Advection

```
type 0
Nx 32
adv_vl 1.0
k 0.0
x_a 0.0
x_b 1.0
C 1.0
```

- type indicates diffusion or advection
- Nx is the gridsize
- adv_vl is the advection velocity
- k is the diffusion coefficient
- x_a is the left boundary
- x_b is the right boundary
- C is the CFL coefficient

## Running Code

In the "code" directory:

put appropriate inputs in "inputs.txt".

#### Writing Data

```bash
./run_advect_diff.ex
```

#### Visualization

Make sure the "input.txt" file is updated, it is read during the visualization process.

```bash
..Project/Code/plotter.py
```
