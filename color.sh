#!/bin/bash
import -window root -crop 1x1+$1+$2 text:- | sed -n 's/.* \(#.*\)/\1/p'