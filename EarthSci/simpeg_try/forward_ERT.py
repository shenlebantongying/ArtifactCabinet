# Ref https://simpeg.xyz/user-tutorials/fwd-dcr-2d/#define-the-topography
# %%
from matplotlib.colors import LogNorm
from simpeg.electromagnetics.static import resistivity
from simpeg.utils import model_builder
from simpeg import maps
from simpeg.electromagnetics.static.utils.static_utils import (
    generate_dcip_sources_line,
    pseudo_locations,
    plot_pseudosection,
    apparent_resistivity_from_voltage,
)

from simpeg.utils import get_default_solver

# discretize functionalityfrom discretize import TreeMesh
from discretize.utils import active_from_xyz
import discretize

# Common Python functionality
import os
import numpy as np
from scipy.interpolate import interp1d
import matplotlib as mpl
import matplotlib.pyplot as plt

# %%

x_topo = np.linspace(-1000, 1000, 400)
z_topo = 50 * np.sin(x_topo / (np.pi * 100)) + 200
topo_2d = np.c_[x_topo, z_topo]

survey_range = [-400, 400]
survey_type = "dipole-dipole"
survey_gap = 40

data_type = "volt"

# survey type
interp_fun = interp1d(x_topo, z_topo)

electrode_loc_x = np.arange(survey_range[0], survey_range[1] + survey_gap, survey_gap)
electrode_loc_z = interp_fun(electrode_loc_x)
electrode_loc = np.c_[electrode_loc_x, electrode_loc_z]
electrode_n = len(electrode_loc_x)

source_list = []

for la in np.arange(0, electrode_n - 3):
    loc_a = electrode_loc[la, :]
    loc_b = electrode_loc[la + 1, :]

    print(la, la + 3)
    locs_m = []
    locs_n = []
    for lm in np.arange(la + 2, electrode_n - 1):
        print(lm, lm + 1)
        locs_m.append(electrode_loc[lm, :])
        locs_n.append(electrode_loc[lm + 1, :])

    print(locs_m)
    print(locs_n)
    print()
    receiver_list = [
        resistivity.receivers.Dipole(
            locations_m=locs_m, locations_n=locs_n, data_type=data_type
        )
    ]

    source_list.append(
        resistivity.sources.Dipole(receiver_list, location_a=loc_a, location_b=loc_b)
    )

survey = resistivity.Survey(source_list)
pseudo_locations_xz = pseudo_locations(survey)

fig, ax = plt.subplots(figsize=(20, 4))

ax.plot(x_topo, z_topo)
ax.scatter(electrode_loc_x, electrode_loc_z, 5, "r")
ax.scatter(pseudo_locations_xz[:, 0], pseudo_locations_xz[:, 1], 5, "g")
fig.show()

# %% Mesh

dh = 4  # base cell width
dom_width_x = 3200.0  # domain width x
dom_width_z = 2400.0  # domain width z
nbcx = 2 ** int(np.round(np.log(dom_width_x / dh) / np.log(2.0)))  # num. base cells x
nbcz = 2 ** int(np.round(np.log(dom_width_z / dh) / np.log(2.0)))  # num. base cells z

# Define the base mesh with top at z = 0 m.
hx = [(dh, nbcx)]
hz = [(dh, nbcz)]
mesh = discretize.TreeMesh([hx, hz], x0="CN", diagonal_balance=True)

# Shift top to maximum topography
mesh.origin = mesh.origin + np.r_[0.0, z_topo.max()]

# Mesh refinement based on topography
mesh.refine_surface(
    topo_2d,
    padding_cells_by_level=[0, 0, 4, 4],
    extrapolate_surface=False,
    finalize=False,
)

# Extract unique electrode locations.
unique_locations = survey.unique_electrode_locations

# Mesh refinement near electrodes.
mesh.refine_points(
    unique_locations, padding_cells_by_level=[8, 12, 6, 6], finalize=False
)

mesh.finalize()

fig, ax = plt.subplots(figsize=(10, 4))
mesh.plot_grid(ax=ax, linewidth=1)
ax.grid(False)
ax.set_xlim(-1000, 1000)
ax.set_ylim(-500, 300)
fig.show()

# %%

active_cells = active_from_xyz(mesh, topo_2d)
n_active = np.sum(active_cells)

# %%

air_conductivity = 1e-8
background_conductivity = 1e-2
conductor_conductivity = 1e-1
resistor_conductivity = 1e-3


# %%
conductivity_model = background_conductivity * np.ones(n_active)


ind_conductor = model_builder.get_indices_sphere(
    np.r_[-120.0, 40.0], 60.0, mesh.cell_centers[active_cells, :]
)
conductivity_model[ind_conductor] = conductor_conductivity

ind_resistor = model_builder.get_indices_sphere(
    np.r_[120.0, 72.0], 60.0, mesh.cell_centers[active_cells, :]
)
conductivity_model[ind_resistor] = resistor_conductivity

# Define log-resistivity model
log_resistivity_model = np.log(1 / conductivity_model)

# %%

# Conductivity map. Model parameters are conductivities for all active cells.
conductivity_map = maps.InjectActiveCells(mesh, active_cells, air_conductivity)

# Resistivity map. Model parameters are log-resistivities for all active cells.
log_resistivity_map = maps.InjectActiveCells(
    mesh, active_cells, 1 / air_conductivity
) * maps.ExpMap(nP=n_active)


plotting_map = maps.InjectActiveCells(mesh, active_cells, np.nan)

fig = plt.figure(figsize=(9, 4))

norm = LogNorm(vmin=1e-3, vmax=1e-1)

ax1 = fig.add_axes([0.14, 0.17, 0.68, 0.7])
mesh.plot_image(
    plotting_map * conductivity_model,
    ax=ax1,
    grid=False,
    pcolor_opts={"norm": norm, "cmap": mpl.cm.RdYlBu_r},
)
ax1.set_xlim(-1000, 1000)
ax1.set_ylim(-300, 300)
ax1.set_title("Conductivity Model")

ax2 = fig.add_axes([0.84, 0.17, 0.03, 0.7])
cbar = mpl.colorbar.ColorbarBase(
    ax2, norm=norm, orientation="vertical", cmap=mpl.cm.RdYlBu_r
)
cbar.set_label(r"$\sigma$ (S/m)", rotation=270, labelpad=15, size=12)

fig.show()

# %%
survey.drape_electrodes_on_topography(mesh, active_cells, topo_cell_cutoff="top")

# %%
solver = get_default_solver()
# DC simulation for a conductivity model
simulation_con = resistivity.simulation_2d.Simulation2DNodal(
    mesh, survey=survey, sigmaMap=conductivity_map, solver=solver
)

# DC simulation for a log-resistivity model
simulation_res = resistivity.simulation_2d.Simulation2DNodal(
    mesh, survey=survey, rhoMap=log_resistivity_map, solver=solver
)


dpred_con = simulation_con.dpred(conductivity_model)

dpred_res = simulation_res.dpred(log_resistivity_model)

print("MAX ABSOLUTE ERROR = {}".format(np.max(np.abs(dpred_con - dpred_res))))

# %%


# Plot voltages pseudo-section
fig = plt.figure(figsize=(8, 2.75))
ax1 = fig.add_axes([0.1, 0.15, 0.75, 0.78])
plot_pseudosection(
    survey,
    dobs=np.abs(dpred_con),
    plot_type="scatter",
    ax=ax1,
    scale="log",
    cbar_label="V/A",
    scatter_opts={"cmap": mpl.cm.viridis},
)
ax1.set_title("Normalized Voltages")
plt.show()

# Get apparent conductivities from volts and survey geometry
apparent_conductivities = 1 / apparent_resistivity_from_voltage(survey, dpred_con)

# Plot apparent conductivity pseudo-section
fig = plt.figure(figsize=(8, 2.75))
ax1 = fig.add_axes([0.1, 0.15, 0.75, 0.78])
plot_pseudosection(
    survey,
    dobs=apparent_conductivities,
    plot_type="contourf",
    ax=ax1,
    scale="log",
    cbar_label="S/m",
    mask_topography=True,
    contourf_opts={"levels": 20, "cmap": mpl.cm.RdYlBu_r},
)
ax1.set_title("Apparent Conductivity")
plt.show()
