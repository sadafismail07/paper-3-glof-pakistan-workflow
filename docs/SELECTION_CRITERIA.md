# SELECTION_CRITERIA.md

## Pre-registered Lake Selection Criteria for GLOF Hydrodynamic Simulation

**Originally finalized:** 2026-04-15
**Last amended:** 2026-04-29 (see Amendment section)

## Purpose

This document records the lake selection rules committed before exposure
analysis and priority scoring were run, in the interest of methodological
transparency and to prevent post-hoc cherry-picking. The selection outputs
will be reported as-run, even if they differ from initial expectations.

## Step 1: Input filtering

1. Restrict to lakes listed in the National Disaster Management Authority
   GLOF-II vulnerable lakes inventory.
2. Filter to NDMA vulnerability rating >= 4 (on the 2-5 scale).
3. Spatially restrict to the Gilgit-Baltistan and Chitral study area, as
   defined by OCHA Pakistan COD-AB v_01 (WFP SDI, 2022) district boundaries.

## Step 2: Per-lake characterisation

For each surviving lake, compile:

- Lake type (moraine-dammed, ice-dammed, supraglacial) from visual
  inspection of Sentinel-2 imagery and published literature.
- Lake area (km^2) from manual digitisation of Sentinel-2 imagery where
  visible, with placeholder zero where not visible.
- Surge-type classification from RGI 7.0 attributes.
- NDMA vulnerability rating (4 or 5).

## Step 3: Per-corridor exposure quantification

For each lake, derive a downstream corridor (50 km cut-off, 1 km lateral
buffer; HydroBASINS Level 9 sub-basin chain via NEXT_DOWN field). Compute:

- Population (GHS-POP R2023A epoch 2025 sum, WorldPop 2020 sum, mean of both)
- Building count (OSM, Geofabrik Pakistan extract)
- Road length km (OSM)
- Bridge count (OSM)
- Built-up area m^2 (GHS-BUILT-S R2023A epoch 2030 sum)
- Power feature count (OSM; reported for completeness, excluded from scoring)

## Step 4: Composite scoring

All scoring variables are log10(x + 1) transformed where right-skewed,
then min-max normalised to the [0, 1] range across the working set.

**Hazard composite** = mean of:
- NDMA vulnerability score (normalised)
- log10(lake area in m^2, treating zero as zero)
- Surge type (binary, RGI 7.0)
- Literature-corroboration indicator (presence/strength of published
  reference for each lake; documented in project verification notes)

**Exposure composite** = mean of:
- Population (GHS+WP mean)
- Building count
- Road length km
- Bridge count
- Built-up area m^2

**Final priority score** = 0.5 * Hazard composite + 0.5 * Exposure composite.

## Step 5: Top-2 selection

The two lakes with the highest final score proceed to detailed hydrodynamic
simulation in HEC-RAS. No tiebreaker is anticipated given the continuous
nature of the composite score; if a tie occurs, the lake with the larger
empirically delineated lake area is selected.

## Step 6: Tie / replacement rule

If one or both top-ranked lakes have zero downstream settlement exposure
(population sum = 0 across both gridded products), it is excluded and
replaced by the next-ranked lake.

## Amendment 2026-04-29

After inspection of the actual NDMA inventory, the following adjustments
were made (before any exposure analysis was performed):

1. The inventory contains 29 NDMA-listed lakes, of which 26 fall within
   the GB + Chitral study area. The original criteria assumed all
   inventory points would be in scope.

2. NDMA vulnerability is a numeric scale (2-5), not categorical. The
   scoring uses min-max normalised numeric values rather than the
   original Low/Medium/High/Very High mapping.

3. The inventory names glaciers (not lakes). For each inventory point,
   the actual lake (where visible) is digitised manually from Sentinel-2
   imagery (Section 3.4 of the manuscript).

4. An additional lake (literature-augmented Khurdopin reference) was
   considered during inventory verification but is not formally part of
   the NDMA working set; treatment of this lake is documented in the
   project notes and is not the subject of the published analysis.

## Commitment

This document was committed to the project repository before the priority
scoring script (scripts/analysis/14_priority_scoring.R) was executed.
The git commit history serves as the timestamp of pre-registration.

