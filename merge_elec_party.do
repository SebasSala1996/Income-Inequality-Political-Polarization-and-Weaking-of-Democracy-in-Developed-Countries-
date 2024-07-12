cd "C:\Users\SEBASTIAN\Documents\MA Social Science\Regimes, State and Institutions\Research proposal and investigation"
use "election_data.dta", clear
merge m:m country_name using election_data.dta
drop _merge
