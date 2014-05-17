#!/bin/sh

{ 
    R -f kleelectrics/kleelectrics.r
} || {
    exec R -f kleelectrics.r
}
