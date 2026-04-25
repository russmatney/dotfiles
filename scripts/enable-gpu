#!/usr/bin/env bash

# allow to load nvidia module
mv /etc/modprobe.d/disable-nvidia.conf /etc/modprobe.d/disable-nvidia.conf.disable

# remove NVIDIA card (currently in power/control = auto)
echo -n 1 > /sys/bus/pci/devices/0000\:01\:00.0/remove
sleep 1
# change PCIe power control
echo -n on > /sys/bus/pci/devices/0000\:00\:01.0/power/control
sleep 1
# rescan for NVIDIA card (defaults to power/control = on)
echo -n 1 > /sys/bus/pci/rescan

# load nvidia module
modprobe nvidia
