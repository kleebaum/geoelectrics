#!/bin/sh

{ 
    R -f /home/anja/kleelectrics/kleelectrics.r
} || {
    exec R -f kleelectrics.r
}
