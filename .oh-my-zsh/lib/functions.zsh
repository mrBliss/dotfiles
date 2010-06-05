# Wiki Lookups
wiki() { dig +short txt $1.wp.dg.cx; }

# mkdir, cd into it
mkcd () {mkdir -p "$*"; cd "$*"}

# Connect to KULeuven
kulssh () {ssh -t s0202013@st.cs.kuleuven.be ssh s0202013@"$*".cs.kotnet.kuleuven.be}