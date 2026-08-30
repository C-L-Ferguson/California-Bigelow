"""
Merge the old CA_Merged_Data_FEB_3.csv (through ~2022) with the new
CJRPROP47 dashboard export (through 2024 Q4).

Strategy:
- Strip leading spaces from Quarter in old file so keys match new file
- Map new file's Q1-Q26 columns to proper names
- Carry forward all DA/election columns from old file for overlapping rows
- New rows (2023-2024) get blank DA columns ready for manual entry
- Recalculate Total_Sentences and percentages for all rows
"""

import csv, math

COL_MAP = {
    'Q1':  'Petition_for_Modification',
    'Q2':  'FTA_Warrant',
    'Q3':  'Prison',
    'Q4':  'Probation',
    'Q5':  'Straight_1170h',
    'Q6':  'Split_1170h',
    'Q7':  'Probation_Petition',
    'Q12': 'MS_Petition',
    'Q18': 'PRCS_Petition',
    'Q26': 'Parole_Petition',
}

DA_COLS = [
    'County.y', 'Name', 'Decarceratory', 'Did_Incumbent_Seek_Reelection',
    'If_Sought_Did_Incumbent_Win', 'Contested', 'Election_Year',
    'Notes...election.years...2014..2018..2022.with.New.DA.coming.in.Q1.after..'
]

def pct(num, denom):
    try:
        n, d = float(num), float(denom)
        if d == 0:
            return ''
        return round(100 * n / d, 6)
    except (ValueError, TypeError):
        return ''

# Load old dataset keyed by (county, quarter_stripped)
old_lookup = {}
old_fieldnames = None
with open('CA_Merged_Data_FEB_3.csv', newline='', encoding='utf-8-sig') as f:
    reader = csv.DictReader(f)
    old_fieldnames = reader.fieldnames
    for row in reader:
        key = (row['County.x'].strip(), row['Quarter'].strip())
        old_lookup[key] = row

# Load new dataset
new_rows = []
with open('/root/.claude/uploads/615b0502-80bb-582c-8c68-427251bda873/ce0fbac4-cjrprop47dashboarddata.csv',
          newline='', encoding='utf-8-sig') as f:
    reader = csv.DictReader(f)
    for row in reader:
        new_rows.append(row)

# Build merged output
output_cols = [
    'County_and_Quarter', 'X', 'County.x', 'Quarter',
    'Petition_for_Modification', 'FTA_Warrant',
    'Prison', 'Probation', 'Straight_1170h', 'Split_1170h',
    'Probation_Petition', 'MS_Petition', 'PRCS_Petition', 'Parole_Petition',
] + DA_COLS + [
    'Total_Sentences', 'Percentage_Prison', 'Percentage_Probation',
    'Percentage_Split', 'Percentage_Straight'
]

merged = []
for i, new_row in enumerate(new_rows):
    county  = new_row['County'].strip()
    quarter = new_row['Quarter'].strip()
    key = (county, quarter)

    out = {col: '' for col in output_cols}
    out['X']             = i + 1
    out['County.x']      = county
    out['Quarter']       = quarter
    out['County_and_Quarter'] = f"{county}_{quarter}"

    # Sentencing columns from new file
    for qcol, named in COL_MAP.items():
        out[named] = new_row.get(qcol, '')

    # DA columns from old file if available
    if key in old_lookup:
        old = old_lookup[key]
        for col in DA_COLS:
            out[col] = old.get(col, '')

    # Recalculate totals and percentages
    prison     = out['Prison']
    probation  = out['Probation']
    straight   = out['Straight_1170h']
    split      = out['Split_1170h']

    try:
        total = int(prison or 0) + int(probation or 0) + int(straight or 0) + int(split or 0)
        out['Total_Sentences']      = total if total > 0 else ''
        out['Percentage_Prison']    = pct(prison,    total)
        out['Percentage_Probation'] = pct(probation, total)
        out['Percentage_Split']     = pct(split,     total)
        out['Percentage_Straight']  = pct(straight,  total)
    except (ValueError, TypeError):
        pass

    merged.append(out)

# Write merged CSV
out_path = 'CA_Merged_Data_2024.csv'
with open(out_path, 'w', newline='', encoding='utf-8') as f:
    writer = csv.DictWriter(f, fieldnames=output_cols)
    writer.writeheader()
    writer.writerows(merged)

print(f"Written {len(merged)} rows to {out_path}")

# Quick validation
matched   = sum(1 for r in new_rows if (r['County'].strip(), r['Quarter'].strip()) in old_lookup)
new_only  = len(new_rows) - matched
print(f"Rows matched with old DA data: {matched}")
print(f"New rows needing DA data entry: {new_only}")

# Show the new quarters that need DA info
new_quarters_only = sorted(set(
    r['Quarter'].strip() for r in new_rows
    if (r['County'].strip(), r['Quarter'].strip()) not in old_lookup
))
print(f"\nQuarters needing DA column fill-in: {new_quarters_only}")
