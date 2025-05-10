#!/bin/sh


cabal install --overwrite-policy=always

/home/zaku/.cabal/bin/asa-exec -y ./asa-exec.yaml


