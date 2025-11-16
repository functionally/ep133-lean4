{
  description = "A development environment for Lean 4 using elan and VS Code.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs2405.url = "github:NixOS/nixpkgs/nixos-24.05";
  };
  outputs = { self, nixpkgs, nixpkgs2405, ... }:
    let
      supportedSystems = [ "x86_64-linux"  ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      mkShellFor = system:
        let
          pkgs = import nixpkgs { 
            inherit system; 
            config = { 
              allowUnfree = true;
            }; 
          };
        in
        pkgs.mkShell {
          buildInputs = with pkgs; [
            lean4
            elan
            vscode
          ];
          shellHook = ''
            echo "Entering Nix shell with Lean and VS Code..."
            export PS1="\[\e[1;32m\][nix-shell:\w]$\[\e[m\] "
          '';
        };
    in
    {
      devShells = forAllSystems (system: {
        default = mkShellFor system;
      });
    };
}
