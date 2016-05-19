# nix-priv-sep
A Nixos tool, by a clueless amateur, for possibly slightly less nonexistent 
privilege separation between various tasks than how I've previously had things 
set up.

This enables a semi-convenient way to set up a user with commands to run under 
'sudo -u' (without a password) on another account (or 'domain') dedicated to 
a particular task. Commands you set up to be run this way are written into 
scripts in ~/bin/ and, if they are X clients, will connect to a Xephyr xserver 
dedicated to their respective domain, in order to prevent programs running 
in different domains from sniffing each others' keyboard traffic etc.

A user set up this way is *not given write access to their home directory*,
in order to keep malicious programs from tampering with configuration. For a 
user 'x', a separate 'x-admin' account is automatically created for the 
purpose of configuring any applications to be run with the privileges of the
main user. Most actual work is intended to be done in the various separate 
domains, whose accounts *do* have write access to their homes.

These domain accounts each have their own home directory in 
'/priv-sep/[user]/[domain]/work', and directories 
'/priv-sep/[user]/[domain]/in' and '/priv-sep/[user]/[domain]/out' for moving 
files in and out.

This is still at a *very, very early*, experimental, stage, but I do use it 
on my own system.

There are likely to be various changes to how exactly the configurations for 
accounts and commands are written. Right now there's no setup for running 
multiple commands from one script, but that should change soon.

**I give no guarantees at all that this design is actually useful for 
protecting anyone from anything.** Qubes OS probably does everything better in 
every way (at the moment nix-priv-sep isn't even containerizing anything), but 
Qubes is not Nixos, and I don't have a computer with enough juice to run 
everything in a separate VM. In case someone does care to take a closer look, 
feedback is welcome, at timo.sinnemaki@helsinki.fi, twitter.com/tpsinnem, 
or 'tpsinnem' on #nixos and elsewhere on freenode.
