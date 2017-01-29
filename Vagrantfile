# -*- mode: ruby -*-
# vi: set ft=ruby:

Vagrant.configure(2) do |config|
    config.vm.box = "minimal/trusty64"
    config.vm.hostname = "dev"
    config.vm.network "private_network", ip: "192.168.50.76"
    config.vm.provision "shell", path: "./provision.sh", privileged: false
    config.vm.provider :virtualbox do |vb|
        vb.customize ["modifyvm", :id, "--usb", "off"]
        vb.customize ["modifyvm", :id, "--usbehci", "off"]
        vb.customize ['modifyvm', :id, '--clipboard', 'bidirectional']
        vb.name = "dev.vagrant"
        vb.memory = 4096
        vb.cpus = 2
    end
end
