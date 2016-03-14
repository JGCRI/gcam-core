#!/bin/bash
## This script sets up the Intel MKL runtime for the PNNL PIC computing
## environment.  The setup may be slightly different on other systems.

module load mkl/15.0.1

## PIC compute nodes have 24 physical processors (i.e., cores), but
## they will report 48 due to hyperthreading.  Hyperthreading isn't
## too useful with BLAS because it is already optimized for cache
## usage.  It is surprisingly hard to get the real number of cores
## through shell commands, so we will just set it by hand here and
## update if the hardware ever changes.
nproc=24

## LAPACK/BLAS are only run from serial code segments, so it's safe to
## use all cores for that.  Other mkl calls might happen from anywhere
## , so we limit those to 1 thread.
MKL_NUM_THREADS=1
MKL_DOMAIN_NUM_THREADS="BLAS=$nproc"
