#!/bin/bash

rsync --exclude=*.swp -zavh . compiler:~/hw2
