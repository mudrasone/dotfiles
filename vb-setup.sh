#!/bin/bash

unload() {
    if [ `ps -ef | grep -c VirtualBox$` -ne 0 ]
    then
        echo "VirtualBox still seems to be running. Please investigate!!"
        exit 1;
    elif [ `ps -ef | grep -c [V]ir` -gt 0 ]
    then
        echo "Stopping running processes before unloading Kernel Extensions"
        ps -ef | grep [V]ir | awk '{print $2}' | xargs kill
    fi
    echo "Unloading Kernel Extensions"
    kextstat | grep "org.virtualbox.kext.VBoxUSB" > /dev/null 2>&1 && \
        sudo kextunload -b org.virtualbox.kext.VBoxUSB
    kextstat | grep "org.virtualbox.kext.VBoxNetFlt" > /dev/null 2>&1 && \
        sudo kextunload -b org.virtualbox.kext.VBoxNetFlt
    kextstat | grep "org.virtualbox.kext.VBoxNetAdp" > /dev/null 2>&1 && \
        sudo kextunload -b org.virtualbox.kext.VBoxNetAdp
    kextstat | grep "org.virtualbox.kext.VBoxDrv" > /dev/null 2>&1 && \
        sudo kextunload -b org.virtualbox.kext.VBoxDrv
}

load() {
    echo "Loading Kernel Extentions"
    sudo kextload "/Library/Application Support/VirtualBox/VBoxDrv.kext" -r \
        "/Library/Application Support/VirtualBox/"
    sudo kextload "/Library/Application Support/VirtualBox/VBoxNetAdp.kext" -r \
        "/Library/Application Support/VirtualBox/"
    sudo kextload "/Library/Application Support/VirtualBox/VBoxNetFlt.kext" -r \
        "/Library/Application Support/VirtualBox/"
    sudo kextload "/Library/Application Support/VirtualBox/VBoxUSB.kext" -r \
        "/Library/Application Support/VirtualBox/"
}

case "$1" in
    unload|remove)
        unload
        ;;
    load)
        load
        ;;
    *|reload)
        unload
        load
        ;;
esac
