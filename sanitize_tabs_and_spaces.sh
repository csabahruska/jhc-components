# removes trailing spaces
find . -name "*.cabal" -type f -print0 | xargs -0 sed -i 's/[[:space:]]*$//'
find . -name "*.hs" -type f -print0 | xargs -0 sed -i 's/[[:space:]]*$//'
find . -name "*.yaml" -type f -print0 | xargs -0 sed -i 's/[[:space:]]*$//'

# expand tabs to spaces
find . -name '*.hs' ! -type d -exec bash -c 'expand "$0" > /tmp/e && mv /tmp/e "$0"' {} \;
