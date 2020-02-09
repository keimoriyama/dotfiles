#!/bin/sh

echo "(#[fg=default]$(uptime | awk '{print $(NF-2)}')#[default]) "
