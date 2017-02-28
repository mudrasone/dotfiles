# -*- mode: ruby -*-
# vi: set ft=ruby:

$script = <<SCRIPT
apt-get install -y mosh-server
exit 0
SCRIPT

Vagrant.configure(2) do |config|
    config.vm.box = "minimal/trusty64"
    config.vm.hostname = "ubuntu.vagrant"
    config.vm.provision "file", source: "~/.ssh/id_rsa.pub", destination: "~/.ssh/brandon.pub"
    config.vm.provision "shell", inline: $script
    config.vm.network "private_network", ip: "192.168.50.52"

    for i in 60000..60010
        config.vm.network :forwarded_port, guest: i, host: i
    end

    config.vm.provider :virtualbox do |vb|
        vb.customize ["modifyvm", :id, "--usb", "off"]
        vb.customize ["modifyvm", :id, "--usbehci", "off"]
        vb.name = "ubuntu.vagrant"
        vb.memory = 4096
        vb.cpus = 4
    end
end
