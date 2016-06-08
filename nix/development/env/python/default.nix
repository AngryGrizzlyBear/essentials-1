#A quick-and-dirty environment for nix-shell that gets you started with python34, pip, and virtualenv.
# Simply run nix-shell with the directory that has this file in it to drop into a shell with everything
# you need.

with import <nixpkgs> {};
with pkgs.python3Packages;

buildPythonPackage {
  name = "pythonEnv";
  buildInputs = [
      cython                        # Cython
      freetype
      gcc                           # GNU Compiler Collection
      gfortran
      git                           # Source control
      atlas                         # ATLAS
      blas                          # BLAS
      openblas                      # OpenBLAS
      libxml2                       # XML2 library
      libxslt                       # XLST library
      libzip                        # Zip library
      liblapack                     # Linear Algebra pack library
      libpng                        # PNG library
      xlibs.libX11                  # X11 library
      pkgconfig
      python3
      python3Packages.virtualenv
      python3Packages.setuptools
      stdenv
      which
      zlib ];
  src = null;
  LAPACK = "test";
  BLAS = "test";
  # When used as `nix-shell --pure`
  shellHook = ''
  unset http_proxy
  export GIT_SSL_CAINFO=/etc/ssl/certs/ca-bundle.crt
  '';
  # used when building environments
  extraCmds = ''
  unset http_proxy # otherwise downloads will fail ("nodtd.invalid")
  export GIT_SSL_CAINFO=/etc/ssl/certs/ca-bundle.crt
  '';
}
