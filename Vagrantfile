#
# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|

  # create mgmt node
  config.vm.define :synereo do |config|
      config.vm.box = "ubuntu/trusty64"
      config.vm.hostname = "synereo"
      config.vm.network :private_network, ip: "10.0.15.10"
      config.vm.provider "virtualbox" do |vb|
        vb.memory = "2048"
      end
      config.vm.provision :shell, path: ".vagrant/bootstrap.sh"
  end
end
