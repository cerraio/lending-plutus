hpack audit
hpack cerra-lending
find audit -name "*.hs" -exec ormolu -i {} \;
cabal build audit
