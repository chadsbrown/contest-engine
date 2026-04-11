# State QSO Party Spec Data Collection

Tracking file for the 13-party initial batch: NM, MO, GA, ND, MI, ON, QC, NE, FL, 7QP, IN, DE, NEQP.

**Goal:** gather all information (rules text, county/district lists, bonus station lists) needed to write contest-engine specs for these parties. This file is the source of truth for what's been collected, what's verified, and what still needs work before a spec can be written.

**Important:** every party in this batch has at least one hard blocker (R, B/C, W, T, or M). None are ready to spec against today's engine — they all need at least the rover-identity gap closed first. This document captures the raw inputs needed *when* engine changes land.

## Legend
- **R** = rover identity (mobile re-workable on county change)
- **B** = post-mult additive bonus points
- **C** = callsign-matched bonus station
- **M** = multi-multiplier per single QSO (one-QSO-multiple-counties exchange)
- **W** = weighted/tiered multiplier (power multiplier, rare-county ×2, grid ÷N)
- **T** = threshold-to-count multiplier (need N QSOs from mult before it counts)
- **S** = sweep/completion bonus

## Overall Status (13 parties)

| # | Party | Rules | Counties | Bonus Stns | Blockers | Notes |
|---|---|---|---|---|---|---|
| 1 | New Mexico | ✅ PDF | ✅ 33 | ✅ W1AW/5 | R, W, B, C, T | Power ×5/×2/×1; mobile +5000/cty (15 QSO threshold); W1AW/5 +250 one-time |
| 2 | Missouri | ✅ PDF | ✅ 115 | ✅ W0MA, K0GQ | R, B, C, T | 100pt bonuses; 40/80m hours +1 (max +250); 50-QSO threshold for mobile mult |
| 3 | Georgia | ✅ HTML | ✅ 159/160 | — | R | 1 Ph / 2 CW; per-mode mults; GA rovers re-workable |
| 4 | North Dakota | ✅ PDF | ✅ 53 | — | R | 1 pt any mode; global mults; mobile-as-new-station |
| 5 | Michigan | ✅ HTML | ✅ 83 | — | R | 1 Ph / 2 CW; per-mode mults; rover re-contactable per cty |
| 6 | Ontario | ✅ PDF | ✅ 50 | ✅ VA3CCO, VE3CCO, VE3ODX, VE3RHQ, VA3RAC | R, B, C, T | +10/contact to bonus stn; mobile/rover +300/area (3-QSO threshold, 3-area min); unique dupe rule "twice per band" |
| 7 | Quebec | ✅ HTML | ✅ 17 regions | — (none from 2026) | R, B, T | 1 Ph / 2 CW per band; mobile +300/region (3-QSO threshold) |
| 8 | Nebraska | ✅ PDF (summary sheet) | ✅ 93 | ✅ NE0QP, + SM, + Appointee | R?, W, W, B, C | Power ×5/×2/×1; **rare grids 5× points**; multiple 100-pt bonus stations; score = (pts × pwr × geo) + bonuses |
| 9 | Florida | ✅ HTML | ✅ 67 | — (1×1 stations for QSO count only) | R, M | 1 Ph / 2 CW; FL mobile new-stn per cty; **CL logs one QSO with multi-cty** |
| 10 | 7QP (7th Call Area) | ✅ HTML | ⚠ 259 (need alt source) | — | R, M | CW 3 / Ph 2 / Dig 4; W7 mobile re-workable per cty; CL sends `ORDES/JEF` multi-cty |
| 11 | Indiana | ✅ HTML | ✅ 92 | — | R | 2 pts any mode (2026 change); per-mode mults; rover re-workable per cty; CL can be logged 2 ways (choose multi-QSO to avoid M) |
| 12 | Delaware | ✅ HTML | ✅ 3 (NDE, KDE, SDE) | — (electronic-submission bonus only) | R, W, B | In-state 1/2/2, out-of-state **10/20/20** (10× built-in); power ×1/×2/×3; +50 e-submission bonus |
| 13 | New England | ✅ HTML | ✅ 68 (CT×9, MA×14, ME×16, NH×10, RI×5, VT×14) | — | R | 1 Ph / 2 CW-Dig; per-band-mode mults for non-NE; NE mobile = new station on cty change |

**13/13 rules verified**; **12/13 county lists complete** (7QP needs alternate source for the full 259-code list).

## Per-party rules detail

### 1. New Mexico QSO Party

- **Sponsor:** ARCC (Amateur Radio Caravan Club)
- **Period:** Second Saturday of April, 1400Z–0200Z Sunday (12 hours)
- **Source:** `NM_forms.pdf` (2026 packet, fetched from http://www.newmexicoqsoparty.org/wp/wp-content/uploads/2026/04/NMQP_Forms.pdf)
- **Cabrillo:** `CONTEST: NM-QSO-PARTY`, `LOCATION: NM` or state/DX
- **Bands:** 160–10 m + 6 m + 2 m (no WARC, 60 m, above 2 m)
- **Modes:** Phone, CW, Digital
- **Exchange:** NM: RS(T) + county; non-NM: RS(T) + State/Province/DXCC
- **Dupe rule:** Once per mode per band
- **QSO points:** Phone 1, CW 2, Digital 2
- **Multipliers (NM stations, global):** 33 NM counties + 50 states + 13 prov + DX entities. Each counted ONLY ONCE regardless of mode/band.
- **Multipliers (non-NM, global):** 33 NM counties, once per contest.
- **Power multiplier:** QRP ≤5w ×5, Low ≤150w ×2, High ×1
- **Mobile rule:** "A New Mexico mobile station that travels into a new county is considered to be a new station and may be contacted again for point and multiplier credit."
- **Bonuses:**
  - Mobile: +5000 pts per NM county from which ≥15 valid QSOs
  - W1AW/5: +250 one-time bonus (2026 only)
- **Score formula:** `(Σ QSO pts × power mult × location mult) + mobile bonus + W1AW/5 bonus`
- **Blockers:** R, W, B, C, T

### 2. Missouri QSO Party

- **Sponsor:** BEARS-St. Louis
- **Period:** Two periods, 1400–0400Z Sat and 1400–2000Z Sun (14 hours)
- **Source:** `MOQP_rules.pdf` (2026 PDF at http://www.w0ma.org/mo_qso_party/results/thisyear/moqp-2026-rules-final.pdf)
- **Bands:** 160–10 m, 6/2/1.25/70cm (no WARC)
- **Modes:** CW, Digital, Phone
- **Exchange:** MO: call + RST + 3-letter county code; non-MO: call + RST + state/prov; DX: call + RST + "DX"
- **Dupe:** Once per mode per band per MO county (for MO-to-MO); once per mode per band (for non-MO-to-MO).
- **QSO points:** Phone 1, CW 2, Digital 2
- **Multipliers (MO):** 115 MO counties + 49 states + 13 prov/territories + 1 DX (if ≥1 DX QSO)
- **Multipliers (non-MO):** 115 MO counties
- **Mobile rule:** "Any mobile or portable category entry that makes 50 or more valid contacts from a county or county lines will be given the multiplier for that county or counties" — threshold.
- **Bonuses:**
  - W0MA: +100 one-time
  - K0GQ: +100 one-time
  - Electronic Cabrillo submission: +100
  - 40m and 80m QSOs during 1400–2000Z Sat and 1400–2000Z Sun: +1 pt each, max +250 total
- **Score formula:** `(QSO pts × mults) + bonuses`
- **Blockers:** R, B, C, T

### 3. Georgia QSO Party

- **Sponsor:** SECC + SEDXC
- **Period:** 2nd full April weekend, 1800Z Sat – 0359Z Sun, then 1400Z – 2359Z Sun
- **Source:** `GA_rules2.html` (https://gaqsoparty.com/georgia-qso-party-rules/)
- **Bands:** 1.8 MHz through 50 MHz (no digital)
- **Modes:** CW, SSB (no digital)
- **Exchange:** GA: signal report + GA county abbreviation; non-GA US: signal report + state; Canada: signal report + province; DX: signal report + "DX"
- **Dupe:** Once per band per mode; rovers once per county per mode per band
- **QSO points:** SSB 1, CW 2
- **Multipliers (GA stations, per-mode):** 50 states + DC + 13 provs = 64; × 2 modes = 128
- **Multipliers (non-GA, per-mode):** 159 GA counties × 2 modes = 318
- **Score formula:** `pts × mults`
- **Mobile:** "Rovers that move to a new county can work everyone again" → R blocker
- **No bonus stations or special scoring.**
- **Blockers:** R (only)

### 4. North Dakota QSO Party

- **Sponsor:** ARRL ND Section (N0RDF + K0YL)
- **Period:** 1800Z–1800Z (24 hours)
- **Source:** `ND_rules.pdf` (http://ndarrlsection.com/2026/2026_nd_qsp_party_rules.pdf)
- **Bands:** 160–10 m + 6/2 m (no WARC)
- **Modes:** SSB/FM (as Phone), CW, Digital (RTTY/PSK; no FT8)
- **Exchange:** ND: RST + county; US/VE: RST + state/prov; DX: RST + country
- **Dupe:** Once per band per mode, AND once per band/mode/county for ND stations
- **QSO points:** 1 pt per contact (all modes)
- **Multipliers (ND, global):** 53 ND counties + 49 states + DC + 10 provs + 3 territories = 116
- **Multipliers (non-ND/DX, global):** 53 ND counties
- **Score:** pts × mults
- **Mobile rule:** "Mobile ND stations that change counties are considered to be a new station" → R blocker
- **CL:** "Mobile stations may park on a county line but each county must be worked in a separate contact" (M avoided)
- **No bonus stations.**
- **Blockers:** R (only)

### 5. Michigan QSO Party

- **Sponsor:** Mad River Radio Club
- **Period:** Saturday of 3rd full April weekend, noon–midnight EDT (12 hours)
- **Source:** `MI_rules.html` (https://miqp.org/index.php/rules/)
- **Bands:** 160–10 m (no WARC)
- **Modes:** CW, SSB
- **Exchange:** MI: RST + county; non-MI W/VE: RST + state/prov; DX: RST + "DX"
- **Dupe:** Once per band per mode (max 10 QSOs per station — 5 bands × 2 modes)
- **QSO points:** SSB 1, CW 2
- **Multipliers (MI, per-mode):** 83 counties + 49 states + DC + 13 prov + DX = 147 × 2 modes
- **Multipliers (non-MI, per-mode):** 83 MI counties × 2 modes
- **Score:** pts × mults
- **Mobile/Rover:** "can be re-contacted when changing counties" → R blocker
- **No bonus stations.**
- **Blockers:** R (only)

### 6. Ontario QSO Party

- **Sponsor:** Contest Club Ontario (CCO)
- **Period:** 3rd full April weekend, 1800Z Sat–0300Z Sun + 1200Z–2000Z Sun
- **Source:** `ON_rules.pdf` (http://www.va3cco.com/oqp/2026OQPRules.pdf)
- **Bands:** 160–2 m (no WARC)
- **Modes:** Phone, CW (all voice = Phone)
- **Exchange:** ON: RS(T) + county abbrev; non-ON: RS(T) + province/state/DX
- **Dupe:** "You may work a station TWICE per band: once on phone and once on CW" — unique; dupe_dimension effectively BandMode but framed differently
- **QSO points:** 2 phone / 2 CW (both 2 pts/contact per band)
- **Bonus stations:** VA3CCO, VE3CCO, VE3ODX, VE3RHQ, VA3RAC worth **10 pts per contact** (new for 2026: VE3RHQ added)
- **Multipliers (ON, per-band):** 50 ON mult areas + 13 provs + states + DXCC
- **Multipliers (non-ON, per-band):** 50 ON mult areas
- **Mobile/Rover bonus:** +300 pts per ON mult area activated; minimum 3 QSOs from that area; minimum 3 areas overall
- **Score:** `(QSO pts × mults) + mobile/rover bonus`
- **Mobile/Rover rule:** "Mobile/Rover stations may be worked again when they change multiplier areas" → R blocker
- **Blockers:** R, B, C, T

### 7. Quebec QSO Party

- **Sponsor:** RAQI
- **Period:** April 19, 2026, 1300–2400 UTC (11 hours)
- **Source:** `QC_rules.html` (https://quebecqsoparty.org/rules-quebec-qso-party/)
- **Bands:** HF (160–10 m, no WARC)
- **Modes:** Phone, CW
- **Exchange:** VE2: RS(T) + administrative region code (3 letters); non-VE2: RS(T) + province/state/DX
- **Dupe:** Once per band per mode
- **QSO points:** Phone 1 / CW 2 per band
- **Multipliers (VE2, per-band):** 17 QC regions + provinces + states + 1 DX
- **Multipliers (non-VE2, per-band):** 17 QC regions
- **Mobile rule:** "When crossing regions, a separate QSO and complete exchange must be made and logged" → multiple QSOs (avoided M); "Mobile stations may contact stations already worked again" → R blocker
- **Mobile bonus:** +300 pts per region activated (min 3 QSOs per region)
- **No bonus stations** (starting 2026)
- **Score:** `(QSO pts × mults) + mobile bonus`
- **Power weighted for club awards:** x2 for low power in club competition (may not apply to main scoring)
- **Blockers:** R, B, T

### 8. Nebraska QSO Party

- **Sponsor:** Nebraska QSO Party Committee (Matt Anderson KA0BOJ)
- **Period:** April 25–26, 2026
- **Source:** `NE_summary.pdf` (https://img1.wsimg.com/blobby/go/6a9e961c-8e45-40ba-bd2a-12a40893a984/downloads/fcf70006-87ea-4ea9-a08c-760bf4c66e6f/Nebraska%20QSO%20Party%20SummarySheet%202025.pdf)
- **Bands:** (assume 160–10 + VHF/UHF; summary sheet mentions satellite)
- **Modes:** Phone, CW, Digital (JT/FT-8/RTTY), Satellite
- **Exchange:** NE: call + RST + county; non-NE: call + RST + state/prov/country
- **Dupe:** Once per band per mode
- **QSO points:** Phone ×1, CW ×3, Digital (JT/FT8/RTTY) ×1, Satellite ×4, Mobile ×3
- **Power multiplier:** QRP (≤5w) ×5, Low (≤150w) ×2, High (>150w) ×1
- **Multipliers (geographic):** 93 NE counties (all stations) + 50 states (NE only) + 13 provs (NE only) + DXCC (NE only) + # grid squares from FT8 QSOs (NE only)
- **Bonuses (flat pts added after final score):**
  - NE0QP bonus: +100
  - Mobile bonus: +100
  - SM (Section Manager) bonus: +100
  - NE Appointee bonus: +50 × worked
  - Rare grid bonus (EN01mi, EN01ni, EN01mh, EN01nh): **5× QSO points** per QSO from those grids
- **Score formula:** `(Σ QSO pts × power mult × geo mult) + bonuses + rare grid extras`
- **Mobile rule:** Not explicit on the summary sheet I could access; likely R blocker (most QSO parties are)
- **Blockers:** W (power ×5/×2/×1), W (rare grid ×5), B (fixed bonuses), C (bonus station callsigns), R (probable)

### 9. Florida QSO Party

- **Sponsor:** FQP
- **Period:** Two 10-hour segments: 1600Z Sat – 0159Z Sun, then 1200Z – 2159Z Sun
- **Source:** `FL_rules.html` (https://floridaqsoparty.org/rules/)
- **Bands:** 160–10 m + 6/2 m
- **Modes:** CW, Phone, Digital
- **Exchange:** FL: cty; non-FL US/KH6/KL7: state; Canada: province; DX/KP4: DXCC prefix; Maritime Mobile: ITU region (1/2/3)
- **Dupe:** Once per mode per band (max 8 per station)
- **QSO points:** Phone 1, CW 2
- **Multipliers (FL, per-mode):** 50 states + DC + 13 prov + DXCC countries + 3 ITU regions maritime
- **Multipliers (non-FL, per-mode):** 67 FL counties (mode-split)
- **Mobile rule:** "Florida Mobiles and Expeditions that move to a new county are considered to be a new station" → R
- **CL rule:** "County-line stations (max 2 counties) may claim separate QSO and multiplier from each county per County Hunter guidelines" — "send all applicable counties in single exchange" → **M blocker**
- **No bonus stations** (1×1 special stations are used for QSO count only, not as mults)
- **Blockers:** R, M

### 10. 7QP (7th Call Area QSO Party)

- **Sponsor:** CODXC
- **Period:** First Saturday of May, 1300Z–0700Z Sun (6 AM–midnight PDT)
- **Source:** `ws7n.net/7QP/new/page.asp?content=rules` — parsed via curl, full text saved earlier
- **Bands:** 160, 80, 40, 20, 15, 10 m
- **Modes:** CW, SSB, Digital (no WSJT — doesn't support exchange)
- **Exchange:** 7th-area: signal report + 5-letter state/county code (e.g., `ORDES`); non-7th-area: signal report + 2-letter state/prov/"DX" code
- **CL station exchange:** Sends **multiple state/county codes** in one exchange, e.g., `UTRIC/IDBEA` (state code needed only once, e.g., `ORDES/JEF`). → **M blocker**
- **Dupe:** Once per band per mode (each band for CW, Phone, and Digital)
- **QSO points:** SSB 2, CW 3, Digital 4. County-line contacts count as multiple QSOs for both stations.
- **Multipliers (7th-area):** 50 states + 13 provs (VE1-9, VO, VY0-2) + up to 10 DXCC entities
- **Multipliers (non-7th-area):** 259 7th-area counties
- **Mobile rule:** "7th-area mobiles may be worked again as they enter new counties" → R
- **No bonus stations or bonus points.**
- **Blockers:** R, M
- **Counties data:** ⚠ **not fetched** (ws7n.net county page returns server errors). Would need to reconstruct from state-level county lists for AZ, CO, ID, MT, NV, OR, UT, WA and assign 5-letter codes per 7QP convention (state-prefix + 3-letter county abbrev). Alternative: N1MM+ ships a 7QP state/county file — grab from N1MM+ GitHub.

### 11. Indiana QSO Party

- **Sponsor:** HDXCC (Hoosier DX and Contest Club)
- **Period:** First full Saturday in May, 1500Z–0259Z Sun (12 hours)
- **Source:** `IN_rules.html` (http://www.hdxcc.org/inqp/rules.html, verified via curl with browser UA)
- **Bands:** 160, 80, 40, 20, 15, 10 m
- **Modes:** Phone, CW
- **Exchange:** IN stations: RS(T) + 5-letter `INxxx` code (e.g., `INMRN` for Marion); non-IN US/Canada: RS(T) + state/prov/territory; DX: RS(T) + "DX"
- **Dupe:** Once per mode per band (max 12 per station for fixed). Mobile/rover: once per mode per band per county (re-workable on county change) → R
- **QSO points:** **2 pts per QSO both modes** (2026 change; was 1 phone / 2 CW)
- **Multipliers (IN, per-mode):** 92 IN counties + 49 other US states + 13 Canadian prov/territories = 154; × 2 modes
- **Multipliers (non-IN, per-mode):** 92 IN counties × 2 modes
- **CL option:** "Contacts with such stations should be logged in either of two ways: as multiple QSOs, one per county; OR as one QSO with each county duly noted." — The multi-QSO option is preferred and **avoids the M blocker**.
- **No bonus stations.**
- **Blockers:** R (only)

### 12. Delaware QSO Party

- **Sponsor:** First State Amateur Radio Club (FSARC)
- **Period:** First full May weekend, 1700Z May 2 – 2359Z May 3 2026 (~31 hours)
- **Source:** `DE_rules_2024.html` (https://www.fsarc.org/qsoparty/rules-2024.html — "current rules" redirect chain eventually lands here)
- **Bands:** All HF except WARC + 6 m+ (including repeater on 6 m+)
- **Modes:** CW, Phone, Digital
- **Exchange:** DE: signal report + county (NDE/KDE/SDE); non-DE: signal report + state/prov/DX
- **Dupe:** Once per band per mode (CW, Phone, Digital all separate)
- **QSO points (in-DE):** Phone 1 / Digital 2 / CW 2
- **QSO points (out-of-DE):** **Phone 10 / Digital 20 / CW 20** — 10× built-in multiplier for out-of-state contacts (workaround for only 3 counties)
- **Multipliers (DE, per-band):** states + prov + DXCC
- **Multipliers (non-DE, per-band):** 3 DE counties (NDE, KDE, SDE)
- **Power multiplier:** High >100W ×1, Low ≤100W ×2, QRP ≤5W ×3
- **Mobile rule:** "Delaware Mobiles that change counties are considered new stations" → R
- **CL rule:** "County line QSOs should be logged as two separate QSOs" (M avoided)
- **Bonus:** +50 pts for electronic log submission (metadata, not in-band)
- **Special FT8/FT4:** Must use Field Day contest overlay in WSJT-X with county-number Class + "DE" section (e.g., `1A DE` for NDE, `2A DE` for KDE, `3A DE` for SDE).
- **Score formula:** `(QSO pts × location mults × power mult) + 50 e-submission bonus`
- **Blockers:** R, W (power mult), B (e-submission bonus, which is metadata-driven and could reasonably be excluded)

### 13. New England QSO Party

- **Sponsor:** YCCC + others
- **Period:** Saturday 2000Z – 0500Z Sun, then 1300Z – 2400Z Sun (20 hours)
- **Source:** `NEQP_rules.html` (https://neqp.org/rules/)
- **Bands:** 80, 40, 20, 15, 10, 6, 2 m (no WARC)
- **Modes:** CW, Phone, Digital
- **Exchange:** NE: signal report + state + county (CT uses "Regional Councils of Government" instead of counties); non-NE: signal report + state/prov; DX: signal report + "DX"
- **Dupe:** "Work New England stations once per band/mode"
- **QSO points:** Phone 1, CW/Digital 2
- **Multipliers (non-NE):** 68 NE counties (CT/9, MA/14, ME/16, NH/10, RI/5, VT/14) — **per band-mode**
- **Multipliers (NE, per-band-mode):** 48 states + 14 Canadian prov/terr + DXCC entities (excluding USA)
- **Mobile rule:** "Mobiles that change counties are considered to be new stations" → R
- **CL rule:** "County-line QSOs logged as two separate contacts" (M avoided)
- **Categories:** SO (HP/LP ≤150w/QRP ≤5w), MO single/multi, Mobile (no MM mobile)
- **No bonus stations.**
- **Blockers:** R (only)

## County / Multiplier Data

### New Mexico (33 counties)

```
BER Bernalillo
CAT Catron
CHA Chaves
CIB Cibola
COL Colfax
CUR Curry
DEB De Baca
DON Dona Ana
EDD Eddy
GRA Grant
GUA Guadalupe
HAR Harding
HID Hidalgo
LEA Lea
LIN Lincoln
LOS Los Alamos
LUN Luna
MCK McKinley
MOR Mora
OTE Otero
QUA Quay
RIO Rio Arriba
ROO Roosevelt
SJU San Juan
SMI San Miguel
SAN Sandoval
SFE Santa Fe
SIE Sierra
SOC Socorro
TAO Taos
TOR Torrance
UNI Union
VAL Valencia
```

### Missouri (115 counties)

Source: `http://w0ma.org/images/MOQP2018/MO-Counties.pdf` → saved as `/tmp/sqp13/MO_counties.pdf`

```
ADR Adair        AND Andrew       ATC Atchison     AUD Audrain
BAR Barry        BTN Barton       BAT Bates        BEN Benton
BOL Bollinger    BOO Boone        BUC Buchanan     BTR Butler
CWL Caldwell     CAL Callaway     CAM Camden       CPG Cape Girardeau
CRL Carroll      CAR Carter       CAS Cass         CED Cedar
CHN Chariton     CHR Christian    CLK Clark        CLA Clay
CLN Clinton      COL Cole         COP Cooper       CRA Crawford
DAD Dade         DAL Dallas       DVS Daviess      DEK DeKalb
DEN Dent         DGL Douglas      DUN Dunklin      FRA Franklin
GAS Gasconade    GEN Gentry       GRN Greene       GRU Grundy
HAR Harrison     HEN Henry        HIC Hickory      HLT Holt
HOW Howard       HWL Howell       IRN Iron         JAC Jackson
JAS Jasper       JEF Jefferson    JON Johnson      KNX Knox
LAC Laclede      LAF Lafayette    LAW Lawrence     LEW Lewis
LCN Lincoln      LIN Linn         LIV Livingston   MAC Macon
MAD Madison      MRE Maries       MAR Marion       MCD McDonald
MER Mercer       MIL Miller       MIS Mississippi  MNT Moniteau
MON Monroe       MGM Montgomery   MOR Morgan       NMD New Madrid
NWT Newton       NOD Nodaway      ORE Oregon       OSA Osage
OZA Ozark        PEM Pemiscot     PER Perry        PET Pettis
PHE Phelps       PIK Pike         PLA Platte       POL Polk
PUL Pulaski      PUT Putnam       RAL Ralls        RAN Randolph
RAY Ray          REY Reynolds     RIP Ripley       SAL Saline
SCH Schuyler     SCT Scotland     SCO Scott        SHA Shannon
SHL Shelby       STC St. Charles  SCL St. Clair    STF St. Francois
STG St. Genevieve STL St. Louis City SLC St. Louis County  STD Stoddard
STN Stone        SUL Sullivan     TAN Taney        TEX Texas
VRN Vernon       WAR Warren       WAS Washington   WAY Wayne
WEB Webster      WOR Worth        WRT Wright
```

### Georgia (159 counties)

Full list in `GA_counties2.html`. Codes are 4-letter uppercase like `APPL`, `BACN`, `FULT`, `MUSC`, etc. 160 codes extracted (one may be a duplicate or bonus entry — needs dedup).

```
APPL ATKN BACN BAKR BALD BANK BARR BART BENH BERR BIBB BLEC BRAN BROK BRYN BULL
BURK BUTT CALH CAND CARR CATO CHAR CHAT CHER CHGA CHTM CLAY CLCH CLKE CLTN CMDN
COBB COFF COLQ COLU COOK COWE CRAW CRIS DADE DAWS DECA DHTY DKLB DODG DOOL DOUG
EARL ECHO EFFI ELBE EMAN EVAN FANN FAYE FLOY FORS FRAN FULT GILM GLAS GLYN GORD
GRAD GREE GWIN HABE HALL HANC HARA HARR HART HEAR HERE HNRY HOUS IRWI JACK JASP
JEFF JENK JFDA JOHN JONE LAMA LANI LAUR LEE LIBE LINC LONG LOWN LUMP MACO MADI
MARI MCDU MCIN MERI MILL MITC MNRO MONT MORG MURR MUSC NEWT OCON OGLE PAUL PEAC
PICK PIER PIKE POLK PULA PUTN QUIT RABU RAND RICH ROCK SCHL SCRE SEMI SPAL STEP
STWT SUMT TALI TATT TAYL TELF TERR THOM TIFT TLBT TOOM TOWN TREU TROU TURN TWIG
UNIO UPSO WALT WARE WARR WASH WAYN WCOX WEBS WFLD WHEE WHIT WILK WKSN WLKR WORT
```

### North Dakota (53 counties)

From `ND_rules.pdf` embedded county list. Raw format in PDF is `Name County-CODE` but some names are multi-word.

```
ADM Adams         BRN Barnes        BSN Benson        BLL Billings
BOT Bottineau     BOW Bowman        BRK Burke         BUR Burleigh
CSS Cass          CAV Cavalier      DIK Dickey        DIV Divide
DUN Dunn          EDY Eddy          EMN Emmons        FOS Foster
GNV Golden Valley GFK Grand Forks   GNT Grant         GRG Griggs
HET Hettinger     KDR Kidder        LMR La Moure      LOG Logan
MCH McHenry       MCI McIntosh      MCK McKenzie      MCL McLean
MCR Mercer        MTN Morton        MRL Mountrail     NEL Nelson
OLR Oliver        PBA Pembina       PRC Pierce        RMY Ramsey
RSM Ransom        REN Renville      RLD Richland      ROL Rolette
SGT Sargent       SRN Sheridan      SIX Sioux         SLP Slope
STK Stark         STL Steele        STN Stutsman      TWR Towner
TRL Traill        WLH Walsh         WRD Ward          WLS Wells
WLM Williams
```

### Michigan (83 counties)

From `MI_counties.html` (https://miqp.org/index.php/official-list-of-mults/). 4-letter codes, with names.

```
ALCO Alcona       ALGE Alger        ALLE Allegan      ALPE Alpena
ANTR Antrim       AREN Arenac       BARA Baraga       BARR Barry
BAY  Bay          BENZ Benzie       BERR Berrien      BRAN Branch
CALH Calhoun      CASS Cass         CHAR Charlevoix   CHEB Cheboygan
CHIP Chippewa     CLAR Clare        CLIN Clinton      CRAW Crawford
DELT Delta        DICK Dickinson    EATO Eaton        EMME Emmet
GENE Genesee     GLAD Gladwin      GOGE Gogebic      GRAT Gratiot
GRTR Grand Traverse HILL Hillsdale HOUG Houghton     HURO Huron
INGH Ingham       IONI Ionia        IOSC Iosco        IRON Iron
ISAB Isabella     JACK Jackson      KALK Kalkaska     KENT Kent
KEWE Keweenaw     KZOO Kalamazoo    LAKE Lake         LAPE Lapeer
LEEL Leelanau     LENA Lenawee      LIVI Livingston   LUCE Luce
MACK Mackinac     MACO Macomb       MANI Manistee     MARQ Marquette
MASO Mason        MCLM Montcalm     MECO Mecosta      MENO Menominee
MIDL Midland      MISS Missaukee    MONR Monroe       MTMO Montmorency
MUSK Muskegon     NEWA Newaygo      OAKL Oakland      OCEA Oceana
OGEM Ogemaw       ONTO Ontonagon    OSCE Osceola      OSCO Oscoda
OTSE Otsego       OTTA Ottawa       PRES Presque Isle ROSC Roscommon
SAGI Saginaw      SANI Sanilac      SCHO Schoolcraft  SHIA Shiawassee
STCL St Clair     STJO St Joseph    TUSC Tuscola      VANB Van Buren
WASH Washtenaw    WAYN Wayne        WEXF Wexford
```

### Ontario (50 multiplier areas)

From `ON_counties.pdf` (OQPMultList.pdf). Format: name / code / section (ONN/ONS/ONE/GH).

```
ALG Algoma District         BRA Brant County          BFD City of Brantford
BRU Bruce County            CHK City of Chatham-Kent  COC Cochrane District
DUF Dufferin County         DUR Durham Regional Mun.  ELG Elgin County
ESX Essex County            FRO Frontenac County      GRY Grey County
HAL Town of Haldimand       HLB Haliburton County     HTN Halton Regional Mun.
HAM City of Hamilton        HAS Hastings County       HUR Huron County
KAW City of Kawartha Lakes  KEN Kenora District       LAM Lambton County
LAN Lanark County           LGR Leeds & Grenville UC  LXA Lennox-Addington
MAN Manitoulin District     MSX Middlesex County      MUS Muskoka District
NIA Niagara Regional Mun.   NIP Nipissing District    NFK Town of Norfolk
NOR Northumberland County   OTT City of Ottawa        OXF Oxford County
PSD Parry Sound District    PEL Peel Regional Mun.    PER Perth County
PET Peterborough County     PRU Prescott & Russell UC PED City of Prince Edward
RAI Rainy River District    REN Renfrew County        SIM Simcoe County
SDG Stormont Dundas Glengarry  SUD Sudbury District   TBY Thunder Bay District
TIM Timiskaming District    TOR City of Toronto       WAT Waterloo Regional Mun.
WEL Wellington County       YRK York Regional Mun.
```

### Quebec (17 administrative regions)

From `QC_rules.html` Appendix I.

```
BSA  1-  Bas Saint-Laurent
SLS  2-  Saguenay-Lac-Saint-Jean
QUE  3-  National Capital
MAU  4-  Mauricie
ETE  5-  Estrie
MTL  6-  Montreal
OTS  7-  Outaouais
ATE  8-  Abitibi-Temiscamingue
CND  9-  Cote-Nord
NDQ  10- Nord-du-Quebec
GIM  11- Gaspesie-iles-de-la-Madeleine
CAS  12- Chaudiere-Appalaches
LVL  13- Laval
LDE  14- Lanaudiere
LNS  15- Laurentides
MEE  16- Monteregie
CDQ  17- Centre-du-Quebec
```

### Nebraska (93 counties)

From `NE_counties_real.pdf` (wsimg CDN).

```
ADMS Adams       ANTE Antelope    ARTH Arthur      BANN Banner
BLAI Blaine     BOON Boone        BOXB Box Butte   BOYD Boyd
BRWN Brown      BUFF Buffalo      BURT Burt        BUTL Butler
CASS Cass       CEDA Cedar        CHAS Chase       CHER Cherry
CHEY Cheyenne   CLAY Clay         COLF Colfax      CUMI Cumming
CUST Custer     DAKO Dakota       DAWE Dawes       DAWS Dawson
DEUE Deuel      DIXO Dixon        DODG Dodge       DGLS Douglas
DUND Dundy      FILL Fillmore     FRNK Franklin    FRON Frontier
FURN Furnas     GAGE Gage         GARD Garden      GARF Garfield
GOSP Gosper     GRAN Grant        GREE Greeley     HALL Hall
HAMI Hamilton   HRLN Harlan       HAYE Hayes       HITC Hitchcock
HOLT Holt       HOOK Hooker       HOWA Howard      JEFF Jefferson
JOHN Johnson    KEAR Kearney      KEIT Keith       KEYA Keya Paha
KIMB Kimball    KNOX Knox         LNCS Lancaster   LINC Lincoln
LOGA Logan      LOUP Loup         MDSN Madison     MCPH McPherson
MERR Merrick    MORR Morrill      NANC Nance       NEMA Nemaha
NUCK Nuckolls   OTOE Otoe         PAWN Pawnee      PERK Perkins
PHEL Phelps     PIER Pierce       PLAT Platte      POLK Polk
REDW Red Willow RICH Richardson   ROCK Rock        SALI Saline
SARP Sarpy      SAUN Saunders     SCOT Scottsbluff SEWA Seward
SHRD Sheridan   SHRM Sherman      SIOU Sioux       STAN Stanton
THAY Thayer     THOM Thomas       THUR Thurston    VLLY Valley
WASH Washington WAYN Wayne        WEBS Webster     WHEE Wheeler
YORK York
```

### Florida (67 counties)

From `FL_counties.pdf` (https://www.floridaqsoparty.org/files/countabb.pdf).

```
ALC Alachua      BAK Baker        BAY Bay          BRA Bradford
BRE Brevard      BRO Broward      CAH Calhoun      CHA Charlotte
CIT Citrus      CLA Clay          CLM Columbia     CLR Collier
DAD Miami-Dade   DES DeSoto       DIX Dixie        DUV Duval
ESC Escambia     FLG Flagler     FRA Franklin     GAD Gadsden
GIL Gilchrist    GLA Glades       GUL Gulf         HAM Hamilton
HAR Hardee       HEN Hendry       HER Hernando     HIG Highlands
HIL Hillsborough HOL Holmes       IDR Indian River JAC Jackson
JEF Jefferson    LAF Lafayette    LAK Lake         LEE Lee
LEO Leon         LEV Levy         LIB Liberty      MAD Madison
MTE Manatee      MAO Marion       MRT Martin       MON Monroe
NAS Nassau       OKA Okaloosa     OKE Okeechobee   ORA Orange
OSC Osceola      PAL Palm Beach   PAS Pasco        PIN Pinellas
POL Polk         PUT Putnam       SAN Santa Rosa   SAR Sarasota
SEM Seminole     STJ St. Johns    STL St. Lucie    SUM Sumter
SUW Suwannee     TAY Taylor       UNI Union        VOL Volusia
WAK Wakulla      WAL Walton       WAG Washington
```

### 7QP (259 codes) ⚠ NOT FETCHED

ws7n.net county page is broken (IIS configuration error). Full 259-code list needs alternate source:
- **Alternate 1:** N1MM+ ships a 7QP state/county database as part of its SectionInfo files. Check `N1MMLogger.net` installation or GitHub mirror.
- **Alternate 2:** Reconstruct from state-level county lists for AZ (15), CO (64), ID (44), MT (56), NV (17), OR (36), UT (29), WA (39) = 300 counties, minus non-7th-area states. But 7QP uses the 5-letter code `<2-letter state><3-letter county>` format — need the specific 3-letter abbrevs used by 7QP which may differ from each state's native QSO party.
- **Alternate 3:** Download the `7qpmults.dat` file some logging software ships with.

### Indiana (92 counties)

From `IN_counties_full.html` (http://www.hdxcc.org/inqp/counties.html). Format: `INxxx` 5-letter code.

```
INADA Adams      INALL Allen       INBAR Bartholomew INBEN Benton
INBLA Blackford  INBOO Boone       INBRO Brown       INCAR Carroll
INCAS Cass       INCLR Clark       INCLY Clay        INCLI Clinton
INCRA Crawford   INDAV Daviess    INDEA Dearborn    INDEC Decatur
INDEK DeKalb     INDEL Delaware    INDUB Dubois      INELK Elkhart
INFAY Fayette    INFLO Floyd       INFOU Fountain    INFRA Franklin
INFUL Fulton     INGIB Gibson      INGRA Grant       INGRE Greene
INHAM Hamilton   INHAN Hancock     INHAR Harrison    INHND Hendricks
INHNR Henry     INHOW Howard     INHUN Huntington  INJAC Jackson
INJAS Jasper     INJAY Jay         INJEF Jefferson   INJEN Jennings
INJOH Johnson    INKNO Knox        INKOS Kosciusko   INLAG Lagrange
INLAK Lake       INLAP LaPorte     INLAW Lawrence    INMAD Madison
INMRN Marion     INMRS Marshall   INMRT Martin     INMIA Miami
INMNR Monroe     INMNT Montgomery INMOR Morgan     INNEW Newton
INNOB Noble     INOHI Ohio (IN)  INORA Orange     INOWE Owen
INPAR Parke     INPER Perry       INPIK Pike        INPOR Porter
INPOS Posey     INPUL Pulaski    INPUT Putnam     INRAN Randolph
INRIP Ripley    INRUS Rush        INSCO Scott       INSHE Shelby
INSPE Spencer  INSTA Starke      INSTE Steuben     INSTJ St. Joseph
INSUL Sullivan INSWI Switzerland INTPP Tippecanoe INTPT Tipton
INUNI Union     INVAN Vanderburgh INVER Vermillion INVIG Vigo
INWAB Wabash    INWRN Warren      INWRK Warrick    INWAS Washington
INWAY Wayne    INWEL Wells       INWHT White      INWHL Whitley
```

Note: Indiana has counties named Delaware, Ohio, and Washington; these conflict with US state postal codes, so the prefix `IN` disambiguates.

### Delaware (3 counties)

```
NDE New Castle
KDE Kent
SDE Sussex
```

### New England QSO Party (68 counties / 6 states)

From `NEQP_counties.html` (https://neqp.org/neqp-county-abbreviations/). Format: 5-letter state+county code.

**Connecticut (9 — uses Regional Councils of Government instead of counties):**
```
CTCAP Capital Region           CTGBR Greater Bridgeport      CTLCR Lower CT River
CTNAU Naugatuck Valley         CTNOE Northeastern            CTNOW Northwest Hills
CTSOE Southeastern             CTSOC South Central            CTWES Western
```

**Massachusetts (14):**
```
MABAR Barnstable   MABER Berkshire    MABRI Bristol       MADUK Dukes
MAESS Essex        MAFRA Franklin     MAHMD Hampden       MAHMP Hampshire
MAMID Middlesex    MANAN Nantucket    MANOR Norfolk       MAPLY Plymouth
MASUF Suffolk      MAWOR Worcester
```

**Maine (16):**
```
MEAND Androscoggin MEARO Aroostook    MECUM Cumberland    MEFRA Franklin
MEHAN Hancock      MEKEN Kennebec     MEKNO Knox          MELIN Lincoln
MEOXF Oxford       MEPEN Penobscot    MEPIS Piscataquis   MESAG Sagadahoc
MESOM Somerset     MEWAL Waldo        MEWAS Washington    MEYOR York
```

**New Hampshire (10):**
```
NHBEL Belknap      NHCAR Carroll     NHCHE Cheshire      NHCOO Coos
NHGRA Grafton     NHHIL Hillsborough NHMER Merrimack    NHROC Rockingham
NHSTR Strafford    NHSUL Sullivan
```

**Rhode Island (5):**
```
RIBRI Bristol      RIKEN Kent         RINEW Newport       RIPRO Providence
RIWAS Washington
```

**Vermont (14):**
```
VTADD Addison      VTBEN Bennington   VTCAL Caledonia     VTCHI Chittenden
VTESS Essex        VTFRA Franklin     VTGRA Grand Isle    VTLAM Lamoille
VTORA Orange       VTORL Orleans      VTRUT Rutland       VTWAS Washington
VTWND Windsor      VTWNH Windham
```

## Bonus-station domain data

The `C` (callsign-match) blocker for several of these parties requires external lists of bonus callsigns. For this batch:

```
# nm_bonus_stations.txt (New Mexico, 2026 only)
W1AW/5    # +250 one-time

# mo_bonus_stations.txt (Missouri)
W0MA      # +100 one-time
K0GQ      # +100 one-time

# on_bonus_stations.txt (Ontario — +10 pts per contact)
VA3CCO
VE3CCO
VE3ODX
VE3RHQ
VA3RAC

# ne_bonus_stations.txt (Nebraska)
NE0QP     # +100
# + SM bonus (Section Manager), +100 — callsign varies by year
# + NE Appointee, +50 each — multiple callsigns
```

Florida, GA, ND, MI, QC (2026+), IN, DE, 7QP, NEQP: **no bonus stations**.

## State / Province / DXCC domain files

These are reusable across parties. `specs/domains/` already has `dxcc_entities.txt` and `arrl_dx_wve_multipliers.txt`. We'll need a new `us_states.txt` (50 postal codes), `ca_provinces.txt` (13 codes: AB, BC, MB, NB, NL/NF+LB, NS, NT/NWT, NU, ON, PE, QC, SK, YT), and a combined `us_ca.txt`.

Notable variations:
- **QC QP** uses `NF/LB` (split) and `NWT` (4 letters) which differs from the standard `NL` / `NT`.
- **NE QP** counts DC as Maryland (`DC` not a valid multiplier).
- **MI QP** counts DC as a state (50+DC).
- **OH QP** counts DC as Maryland.
- **NM QP** counts DC as Maryland.

## Engine-gap summary (this batch)

All 13 parties hit at least the **R** (rover identity) blocker. Of the 13, exactly **4** are R-only and would be immediately unblocked by a rover-identity fix alone: **GA, ND, MI, IN**. **NEQP** is also essentially R-only (county-line is logged as 2 QSOs, no bonuses).

The remaining 8 parties need additional gaps:
- **FL, 7QP** — also need M (multi-mult per QSO)
- **NM, MO, QC, ON, NE, DE** — also need B + C (+ T in some cases) for bonus stations and post-mult additive scoring; **NE** and **DE** additionally need W (power multiplier) or grid-rarity ×5; **NM** needs W (power multiplier)

## Next steps

1. **Verify 7QP counties** via N1MM+ or alternative. Full 259-code list is the only missing dataset in this batch.
2. **Dedup GA county list** — 160 codes extracted but the party advertises 159. Cross-check against the official list.
3. **Engine work, in order:**
   - **R (rover identity)** — unlocks GA, ND, MI, IN, NEQP (5 parties) immediately.
   - **B + C + T (bonus rules)** — unlocks NM, MO, QC, ON (4 more).
   - **W (power/weight multiplier)** — unlocks DE, NE (and improves NM coverage).
   - **M (multi-mult per QSO)** — unlocks FL, 7QP.
4. **After R lands:** write `specs/gaqp.json`, `specs/ndqp.json`, `specs/miqp.json`, `specs/inqp.json`, `specs/neqp.json` plus their respective domain files first — they're the fastest to land and validate the rover-identity implementation end-to-end.
5. **Raw data snapshots** are at `/tmp/sqp13/` — ephemeral; if any get flushed, all the URLs are captured above and re-fetching with a browser User-Agent works for everything except DE (qsl.net redirect chain) and 7QP counties (broken IIS config).
