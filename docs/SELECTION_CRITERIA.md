# SELECTION_CRITERIA.md

## Pre-registered Lake Selection Criteria for GLOF Hydrodynamic Simulation

**Originally finalized:** 2026-04-15
**Last amended:** 2026-08-28 (see Amendment section)

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

5. Spelling note: the NDMA inventory spells the L15 working-set lake
   "Khurdupin" (data/raw/ndma_vulnerable_glof_sites.csv), and this
   spelling is retained as-is throughout the tables, figures and scripts
   in this repository. This is not a typo for "Khurdopin" -- the two
   names refer to different, unconnected features roughly 208 km apart
   (L15 is in Yasin valley, Gupis-Yasin district; the published
   Khurdopin Glacier is in Shimshal valley, Hunza district). See
   manuscript Section 3.3 and the Table S4 footnote for the full
   identification check. Do not "correct" one spelling to the other.

## Amendment 2026-08-28 (post-peer-review)

Following comments from Reviewer 3 (Comment R3.13) and the editor's request to
ensure the hazard composite measures physical hazard rather than data
availability, two changes were made to Step 4's hazard composite scoring
after the original submission. As with the 2026-04-29 amendment, this is
recorded here rather than silently edited into Step 4 above, and the
resubmission's response-to-reviewers letter is the primary record of the
review comment that prompted it.

1. **Literature-corroboration indicator removed.** Step 4 as originally run
   included a fourth hazard-composite component, a "literature-corroboration
   indicator" reflecting the presence and strength of a published reference
   for each lake. On reflection this is a measure of how well-documented a
   lake is, not of its physical outburst hazard, and a data-availability
   measure has no place in a hazard composite. It has been dropped. The
   hazard composite is now the mean of three physical indicators only: NDMA
   vulnerability rating (normalised), lake surface area (log10, m^2), and
   RGI 7.0 surge-type classification (binary).

2. **Surge-type credit restricted to lakes that are themselves ice-dammed.**
   The surge-type indicator previously credited any lake associated with a
   surge-type glacier, including moraine-dammed lakes fed by a surging
   tributary. A surge in a tributary glacier does not itself alter a
   moraine dam's failure mode, so surge-type credit is now restricted to
   lakes that are themselves ice-dammed. L29 Passu (moraine-dammed) and L26
   Hundur no longer receive surge-type credit under the adopted composite.

Both changes were made before the resubmission's Table 2 ranking was
finalised, and the full working-set ranking was recomputed under the revised
composite. The top-two selection is unchanged: L29 Passu ranks first and L27
Shisper ranks second, so neither lake selected for hydrodynamic simulation is
affected by this amendment.

## Commitment

This document was committed to the project repository before the priority
scoring script (scripts/analysis/14_priority_scoring.R) was executed.
The git commit history serves as the timestamp of pre-registration.

