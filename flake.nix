{
  description = "Stamno Media — monochrome terminal-style streaming UI in Elm";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      tachyons = pkgs: pkgs.fetchurl {
        url = "https://unpkg.com/tachyons@4.12.0/css/tachyons.min.css";
        sha256 = "10ha7rkf939n84n2zi0swm8q5d85lqgdbzhlrraqkvss5pk1y09j";
      };

      # hls.js for HLS VOD playback (transcode PlayMethod is served as
      # /videos/<id>/main.m3u8 by the server).
      hlsJs = pkgs: pkgs.fetchurl {
        url = "https://unpkg.com/hls.js@1.5.17/dist/hls.min.js";
        sha256 = "sha256-SEBU6M0D0/bReB+39AK9wxjYpMUn+TOpXGJOJ8yalHA=";
      };

      # Pre-populates ELM_HOME so elm/elm-test-rs run offline in the sandbox.
      elmDeps = pkgs: pkgs.elmPackages.fetchElmDeps {
        elmPackages = import ./elm-srcs.nix;
        elmVersion = "0.19.1";
        registryDat = ./registry.dat;
      };

      src = builtins.path {
        path = ./.;
        name = "stamno-media-src";
        filter = path: _type:
          !(builtins.elem (baseNameOf path) [
            "result"
            "elm-stuff"
            "design_handoff_movie_streaming_ui"
            "Simple movie streaming UI.zip"
          ]);
      };
    in
    {
      packages = forAllSystems (pkgs: rec {
        default = stamno-media;

        stamno-media = pkgs.stdenv.mkDerivation {
          pname = "stamno-media";
          version = "0.1.0";
          inherit src;

          nativeBuildInputs = [ pkgs.elmPackages.elm ];
          configurePhase = elmDeps pkgs;

          buildPhase = ''
            elm make src/Main.elm --optimize --output=elm.js
          '';

          installPhase = ''
            mkdir -p $out
            cp elm.js public/index.html public/stamno.css $out/
            cp ${tachyons pkgs} $out/tachyons.min.css
            cp ${hlsJs pkgs} $out/hls.min.js
          '';
        };
      });

      checks = forAllSystems (pkgs: {
        build = self.packages.${pkgs.stdenv.hostPlatform.system}.default;

        elm-test = pkgs.stdenv.mkDerivation {
          name = "stamno-media-elm-test";
          inherit src;

          nativeBuildInputs = [
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-test-rs
            pkgs.nodejs
          ];
          configurePhase = elmDeps pkgs;

          buildPhase = ''
            elm-test-rs --offline
          '';

          installPhase = "touch $out";
        };
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = [
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-test-rs
            pkgs.elmPackages.elm-language-server
            pkgs.elmPackages.elm-live
            pkgs.elm2nix
            pkgs.nodejs
          ];
        };
      });

      apps = forAllSystems (pkgs: {
        # nix run — live-reloading dev server over the working tree
        default = {
          type = "app";
          program = toString (pkgs.writeShellScript "serve-stamno-media" ''
            set -e
            mkdir -p .dev
            ln -sf "$PWD"/public/index.html "$PWD"/public/stamno.css .dev/
            ln -sf ${tachyons pkgs} .dev/tachyons.min.css
            ln -sf ${hlsJs pkgs} .dev/hls.min.js
            exec ${pkgs.elmPackages.elm-live}/bin/elm-live src/Main.elm \
              --dir=.dev --host=127.0.0.1 --port=8000 \
              --path-to-elm=${pkgs.elmPackages.elm}/bin/elm \
              -- --output=.dev/elm.js
          '');
        };
      });
    };
}
