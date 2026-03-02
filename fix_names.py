import os
import re
import glob

pkg_dir = '/Users/ricardoochoasosa/Git/urbanperformance'
r_dir = os.path.join(pkg_dir, 'R')

def fix_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # Replace pipe
    content = content.replace('%>%', '|>')

    # Note: we are NOT removing return() automatically because it's hard to distinguish early returns 
    # from final returns reliably with regex. We will do it in a targeted way or manually later.

    # Fix dot-notation variable and function names to snake_case. 
    # Only known exported functions and internal variables specific to this package.
    replacements = {
        'agricultural.land.consumption': 'agricultural_land_consumption',
        'amenities.checker': 'amenities_checker',
        'amenities.proximity': 'amenities_proximity',
        'biodiversity.land.consumption': 'biodiversity_land_consumption',
        'cycle.proximity': 'cycle_proximity',
        'cycle.track.density': 'cycle_track_density',
        'green.area.pcapita': 'green_area_pcapita',
        'green.land.consumption': 'green_land_consumption',
        'intersection.density': 'intersection_density',
        'jobs.proximity': 'jobs_proximity',
        'land.consumption': 'land_consumption',
        'land.cover.areas': 'land_cover_areas',
        'land.cover.loss': 'land_cover_loss',
        'non.cero': 'non_cero',
        'one.cero': 'one_cero',
        'one.na': 'one_na',
        'population.density': 'population_density',
        'population.in.risk': 'population_in_risk',
        'public.transport.proximity': 'public_transport_proximity',
        'roads.density': 'roads_density',
        'roads.length': 'roads_length',
        'tot.pop': 'tot_pop',
        'urba.footprint': 'urban_footprint',
        'urban.footprint': 'urban_footprint',
        # Inner variables
        'agri.b': 'agri_b',
        'agri.c': 'agri_c',
        'agri.r': 'agri_r',
        'buildup.b': 'buildup_b',
        'bl.b': 'bl_b',
        'bl.c': 'bl_c',
        'bl.r': 'bl_r',
        'cycle.r': 'cycle_r',
        'cycle.d': 'cycle_d',
        'pop.prox.cycle': 'pop_prox_cycle',
        'cycle.reclass': 'cycle_reclass',
        'green.area': 'green_area',
        'green.area.r': 'green_area_r',
        'green.area.t': 'green_area_t',
        'green.area.p': 'green_area_p',
        'greenl.b': 'greenl_b',
        'greenl.c': 'greenl_c',
        'greenl.r': 'greenl_r',
        'p.scenario': 'p_scenario',
        'p.base': 'p_base',
        'area.in': 'area_in',
        'infill.a': 'infill_a',
        'n.inter': 'n_inter',
        'u.footprint': 'u_footprint',
        'inter.dens': 'inter_dens',
        'j.mean': 'j_mean',
        'j.sd': 'j_sd',
        'j.value': 'j_value',
        'j.dist': 'j_dist',
        'j.prox': 'j_prox',
        'fp.base': 'fp_base',
        'fp.horizon': 'fp_horizon',
        'l.consumption': 'l_consumption',
        'r.consumption': 'r_consumption',
        'dist.r': 'dist_r',
        'dist.reclass': 'dist_reclass',
        'param.row': 'param_row',
        'param.value': 'param_value',
        'dist.rasters.list': 'dist_rasters_list',
        'r.proximity': 'r_proximity',
        'distance.stack': 'distance_stack',
        'pop.tot': 'pop_tot',
        'urban.fp': 'urban_fp',
        'pop.dens': 'pop_dens',
        'r.pop': 'r_pop',
        'pop.prox.transit': 'pop_prox_transit',
        'transport.p': 'transport_p',
        'transport.s': 'transport_s',
        'amen.s': 'amen_s',
        'amen.p': 'amen_p',
        'amen.args': 'amen_args',
        'distance.stack': 'distance_stack'
    }

    # Replace word boundaries for these variables
    for old, new in replacements.items():
        content = re.sub(r'\b' + old.replace('.', r'\.') + r'\b', new, content)

    with open(filepath, 'w') as f:
        f.write(content)

# Process all R files
for r_file in glob.glob(os.path.join(r_dir, '*.R')):
    fix_file(r_file)

# Also process vignette
vig_file = os.path.join(pkg_dir, 'vignettes/urbanperformance.Rmd')
if os.path.exists(vig_file):
    fix_file(vig_file)

print("Done replacing.")
