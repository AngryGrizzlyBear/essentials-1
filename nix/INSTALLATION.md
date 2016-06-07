# INSTALLING ENCRYPTED NIX OS

This is just a random jumbling of notes I took when I installed Nix on my T440s.

## Partitioning

I used the graphical installer and gparted to do partitioning.


First, the partition table should be GPT.

1MB grub data partition - EXT4 and bios_grub flag set. I called this partition "grub". Make sure to click "apply".
Otherwise it will not let you right click -> manage flags to set `bios_grub` to true.

1GB boot partition - on the large side but I have 500GB to spend. Set this to fat32. Flags are "boot" and "esp".

Finally use the remaining space in one big partition, format it to EXT4 with the lvm flag, and commit it.

## Disk Encryption

The large LVM partition above is our encrypted partition.

Type `cryptSetup luksFormat /dev/sdX3` where `sdX` is the drive name. Enter a good password. Not used anywhere
else.

Now we need to create the partitions

`cryptsetup luksOpen /dev/sdX3 enc-pv` and enter the password from above.

`pvcreate /dev/mapper/enc-pv` to initialize the partition.

`vgcreate vg /dev/mapper/enc-pv` to create the volume group `vg` on `/dev/mapper/enc-pv` from above.

`lvcreate -L 16GB -n swap vg` to create a swap logical volume on vg. I went with 2x RAM_SIZE_IN_GB. This is a little
excessive and can be reduced quite a bit.

`lvcreate -l 100%FREE -n root vg` to create the root logical volume on vg. This consumes the remaining space in the
volume group.

## Partition Formatting and Mounting

Since we used `gparted` it is not necessary to format the unencrypted partitions.

For `root` however we need to format it `ext4` (or whatever format you want) and also configure the swap.

`mkfs.ext4 -0 dir_index -j -L root /dev/vg/root` to format the root as `ext4`.

`mkswap -L swap /dev/vg/swap`.

Now let's mount these for installation:

`mount /dev/vg/root /mnt`
`mkdir /mnt/boot`
`mount /dev/sdX2 /mnt/boot`
`swapon /dev/vg/swap`

## Initial Nix Configuration

Now we need to set up a barebones configuration so we can see if we can boot up.

`nixos-generate-config --root /mnt`

This will give us two files, `/etc/nixos/hardware-configuration.nix` and `/etc/nixos/configuration.nix` to set the
system up with.

The defaults are fairly sane for an initial boot. Double check to make sure everything looks okay according to
your system specs.

The major change we need to make is to let NixOS know we have an encrypted partition. Add the following into the
`configuration.nix` file.

```
boot.initrd.luks.devices = [
  { name = "root"; device = "/dev/sdX3"; preLVM = true; }
];
```

I tend to keep my boot stuff grouped together near the top.

We will keep the `gummiboot` related lines. `gummiboot` is a good enough bootloader for our purposes.

In your `hardware-configuration.nix` file your boot partition may not have the right `fsType`. Change it to `vfat` if
necessary.

Finally, add `networking.wireless.enable = true;` to the file so we can use `wpa_supplicant` when we boot back up.

now type `reboot` to reboot. You should see gummiboot.

## Installing the Rest of the System

Now that the hardest part is over we can configure Nix.

Let's begin by setting up a user.

`useradd -m <your_name>`
`passwd <your_name>`
`groupadd -a -G wheel <your_name>`

This will grant your user access to everything under sudo.
