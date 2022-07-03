## wwtools-ada
These are theoretically-functional Ada tools to work with WWise banks and hashes.

## wwbrute, wwdict, wwgen
wwbrute will brute force a given FNV32 hash using as many threads as you configure.
wwdict will attempt a dictionary attack on an FNV32 hash using a specified dictionary.
wwgen simply calculates an FNV32 hash based upon strings provided.

## wwmap
wwmap takes an output file from the above tools and will apply it to a list of hashes given on standard input,
transforming those for which there are known names, and leaving alone those without names.  

## wwparse
wwparse takes (multiple) WWise bnk files as input and will output corresponding raw objects in a more generic format.
Unlike wwiser, which produces playable txtps, this is designed to rapidly produce details which would be useful for someone reassembling 
tracks.
