#!/usr/bin/env bash

function fdate() {
    date +'%Y-%m-%d %H:%M:%S %p';
}

function battery()
{
    acpi -b |
        # First battery only
        head -n 1 |
        sed "s/^Battery [0-9]*: //" |
        sed "s/^Discharging, /    🔋 /" |
        sed "s/^Charging, /    🔌 /" |
        sed "s/%.*$/%    /"
}

while true
do
    echo "$(battery) $(fdate)";
    sleep 1
done
