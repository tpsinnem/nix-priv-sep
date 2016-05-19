# An example configuration.

{ config, pkgs, ... }:

let psPkgs = import ./priv-sep-pkgs; in

{
  imports = [ ./priv-sep-pkgs/priv-sep/default.nix ];

  nixpkgs.config.packageOverrides = pkgs: {
     slim = psPkgs.slim; 
  };

  users.privSepUsers = [
    { name = "alice";
      domains = [

          # The name of the domain account, which gets a home
          # in /priv-sep/alice/notes/work
        { name = "notes";
          commands = [

              # A command to be run. The executable must be given as an
              # absolute path.
            { command = "${pkgs.fluxbox}/bin/fluxbox";
                              
              # The name of the script for this command. Gets written
              # to /home/alice/bin/notes.
              alias = "notes"; } ];
        }
        { name = "web";
          commands = [
            { command = "${pkgs.fluxbox}/bin/fluxbox";
              alias = "web"; } ];
        }
        { name = "project-foo";
          commands = [
            { command = "${pkgs.fluxbox}/bin/fluxbox";
              alias = "foo"; } ];
        }
      ];
    }
    { name = "bob";
      domains = [
        { name = "project-foo";
          commands = [
            { command = "${pkgs.i3}/bin/i3";
              alias = "foo"; } ];
        } 
      ];
    }
 ];  

}
