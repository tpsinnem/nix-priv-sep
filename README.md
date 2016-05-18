# nix-priv-sep
Some Nixos tools for possibly slightly less nonexistent privilege separation 
between various tasks than in my previous status quo.

These enable a semi-convenient way to set up for a given user to be able to 
'sudo -u' (without a password) to another account (or 'domain') dedicated to 
a particular task. The command you set up to be run this way will connect to a 
Xephyr xserver dedicated to that domain in order to prevent programs running 
in different domains from sniffing each others' keyboard traffic etc.

These domain accounts each have their own home directory in 
'/priv-sep/[user]/[domain]/work', and directories 
'/priv-sep/[user]/[domain]/in' and '/priv-sep/[user]/[domain]/out' for moving 
files in and out.
