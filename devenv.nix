{ pkgs, lib, config, inputs, ... }:

{
  packages = [ pkgs.jet
               pkgs.babashka
               pkgs.emacsPackages.dash
               pkgs.emacsPackages.ht
               pkgs.emacsPackages.s
               pkgs.emacsPackages.parseedn
             ];
}
