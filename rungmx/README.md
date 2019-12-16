# RUNGMX - Automated Generic GROMACS MD Run Script

## Synopsis
Simple shell script for automatically running GMX simulations.  
Q: Why?  
A: Because I am lazy...

## Build & Usage
In main directory type:
```bash
make
make test
```
To install program type following as *root* (or *sudo*):
```bash
make install
```
To use the program type `rungmx [-options] [run files]`, where the options are:
```
-f     -- MDP files path.
-c     -- Structure file.
-p     -- Topology file.
-log   -- Name of logfile.
```
Example:  
```bash
gmxrun -c conf.gro -f mdp/ min eq prod
```

## License
-N/A-
