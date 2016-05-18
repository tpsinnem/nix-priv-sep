{ config, lib, pkgs, ... }:

with lib;

let util = (import ./util.nix { inherit lib pkgs; }); in

{
  options = {

    users.privSepUsers = mkOption {

      default = [];

      type = types.listOf ( types.submodule { options = {

        name = mkOption {
          type = types.string;
          description = "User name, also serves as base for derived names [name]-admin, [name]-[domain] etc."; };

        # uid = mkOption {
        #   type = types.int;
        #   description = "User id."; };

        # gid = mkOption {
        #   type = types.int;
        #   description = "Group id for user's eponymous primary group."; };

        # adminUid = mkOption {
        #   type = types.int;
        #   description = "User id for an administrative system account for this user."; };

        domains = mkOption {

          type = types.listOf ( types.submodule { options = {

            name = mkOption {
              type = types.string;
              description = "Name of domain. Gets its own system account, with domain name appended" + 
                            "to proper username, and a home directory at" + 
                            "/priv-sep/[username]/[domainname]."; };

            # uid = mkOption {
            #   type = types.int;
            #   description = "User id for domain-specific system account."; };

            # gid = mkOption {
            #   type = types.int;
            #   description = "Group id for domain-specific group."; };

            # xIsoUid = mkOption {
            #   type = types.int;
            #   description = "User id for X isolation system account."; };

            commands = mkOption {

              type = types.listOf ( types.submodule { options = {
                command = mkOption {
                  type = types.string;
                  description = "Command to run."; };
                alias = mkOption {
                  type = types.string;
                  description = "Alias for the command."; };
              }; } );
              
              description = "Commands the user is to be able to run in the domain.";
            };

          }; } );

          description = "Privilege separation domains for the user.";
        };

      }; } );
    };
  };

  config = {

    users.users = foldl util.addPrivSepUser {} config.users.privSepUsers;

    users.groups = foldl util.addUserGroupAndDomainGroups {} config.users.privSepUsers;

    security.sudo.extraConfig = ''
      Defaults always_set_home
      '' +
      foldl util.addSudoCfg "" config.users.privSepUsers;

                 
    system.activationScripts = { 

      privSepDirs = {

        deps = ["users"];
        text = ''
          mkdir -p /priv-sep
          chmod go+rx /priv-sep
          '' +
          foldl util.addDirScripts "" config.users.privSepUsers;

      };

      privSepUserHomes = {
        deps = ["users"];
        text = 
          foldl
            ( script: psu:
              script + ''
              touch /home/${psu.name}/.privSepUser
              touch /home/${psu.name}/.xsession-errors
              chmod g+rw /home/${psu.name}/.xsession-errors
              '' )
            ""
            config.users.privSepUsers;
      };

      sudoCommands = {
        deps = ["users"];
        text = foldl util.addSudoCommands "" config.users.privSepUsers;
      };
    };

    services.xserver.displayManager.sessionCommands = ''
      if [[ -f ~/.privSepUser ]]; then
        /run/current-system/sw/bin/chmod g+r ~/.Xauthority
      fi
    '';
  };
}
