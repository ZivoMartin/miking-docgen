import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

# Installation
This page covers how to install and run Miking natively on your system. See
[other installation methods](#other-installation-methods) at the bottom of this page for options.

## Install Miking
:::info Required Dependencies

**You need to install Opam** (OCaml package manager)
before you can use the Miking system. See the
[Opam installation guide](https://opam.ocaml.org/doc/Install.html) for
how to install Opam on Mac or on your Linux distribution:
[Link](https://opam.ocaml.org/doc/Install.html)

For Windows, install Opam using Windows Subsystem for Linux (WSL).

:::

With `opam` installed, create a switch for Miking and install the packages
`dune` and `linenoise`. Do that by running the following:

```
opam update
opam switch create miking-ocaml 5.0.0
eval $(opam env)

opam install dune linenoise menhir ocamlfind
```

:::tip If Using Multiple Opam Switches

Note that the `opam switch` command lets you have several OCaml installations on
your system. When using the Miking system, you need to use the `miking-ocaml`
switch. Running `opam switch miking-ocaml` followed by `eval $(opam env)`
always takes you back to the correct switch.

:::

Clone the Miking git repository to a suitable location on your system and enter
the directory:

```
git clone https://github.com/miking-lang/miking.git
cd miking
```

If you do not have `git` installed on your system then you can manually
download a zip file with the source code from our GitHub page:
[https://github.com/miking-lang/miking/archive/refs/heads/develop.zip](https://github.com/miking-lang/miking/archive/refs/heads/develop.zip)

Assuming that you have downloaded the repository and are currently located in
that directory, then you can either install Miking on your system with `make`
or directly using your shell:

:::info Install

<Tabs>
<TabItem value="mi-install-makefile" label="Makefile" default>

With `make` available, you can install Miking as:

```
make install
```
</TabItem>

<TabItem value="mi-install-shell" label="Shell">

If `make` is not available on your system, then you can directly install Miking
by running the provided `make.sh` shell script as:

```
./make.sh boot && ./make.sh install-boot && ./make.sh && ./make.sh install
```
</TabItem>
</Tabs>

:::

This will install the `mi` binary under `$HOME/.local/bin`, as well as the
standard library under `$HOME/.local/lib`. If not already, make sure that
`$HOME/.local/bin` is on your shell's PATH by running the following command
directly or adding it to the appropriate .rc-file (e.g. `.bashrc` or `.zshrc`):

```bash
export PATH="$HOME/.local/bin:$PATH"
```

**This concludes the basic installation.** You can now compile and run MCore
programs using `mi compile`:

```bash
> echo 'mexpr print "Hello, Miking!\n"' > hello.mc
> mi compile hello.mc
> ./hello
Hello, Miking!
```

See the [documentation](/docs) page for more usage.

:::note Uninstall

<Tabs>
<TabItem value="mi-install-makefile" label="Makefile" default>

To uninstall Miking, run the following in the cloned Miking repository:

```
make uninstall
```
</TabItem>

<TabItem value="mi-install-shell" label="Shell">

To uninstall Miking, run the following in the cloned Miking repository:

```
./make.sh uninstall
```
</TabItem>
</Tabs>

:::

## Optional Dependencies

Certain parts of the standard library depends on additional OCaml packages to
properly function. See below for how to install them for your system:

:::tip Optional Dependencies

<Tabs>
<TabItem value="optdep-linux" label="Linux" default>

Install the dependencies with Opam:

```
opam install pyml toml lwt owl.0.10.0 ocamlformat.0.24.1 conf-openblas.0.2.1
```

Opam should invoke your distribution's package manager if there are any
additional dependencies needed.

</TabItem>
<TabItem value="optdep-macos-x86" label="Mac (Intel x86)">

It might be necessary to add openblas to the pkg-config path if installed with
Homebrew. Replace `/usr/local` in the `PKG_CONFIG_PATH` variable below with
your Homebrew prefix if they differ.

```bash
# Install openblas
brew install openblas

# Double check if /usr/local is what your Homebrew installation uses as its
# prefix. Change if it is different.
export PKG_CONFIG_PATH="/usr/local/opt/openblas/lib/pkgconfig:${PKG_CONFIG_PATH}"

# Now install the dependencies with Opam:
opam install pyml toml lwt owl.0.10.0 ocamlformat.0.24.1 conf-openblas.0.2.1
```

</TabItem>
<TabItem value="optdep-macos-arm64" label="Mac (Apple Silicon)">

Before installing the Opam packages, it is most likely necessary to perform
the following steps before proceeding with the installation:

```bash
# Install gcc and openblas
brew install gcc openblas

# Make sure that you have gcc as your cc compiler (change version from 12 to
# your GCC version if it is different)
cd $HOMEBREW_PREFIX/bin && ln -s gcc-12 cc
```

Now verify that your `cc` compiler points to the correct location by running
`which cc`. **You might need to open a new terminal window for this.**

Once `cc` points to the correct location, run the following commands to install
the remaining dependencies:

```bash
export PKG_CONFIG_PATH="$HOMEBREW_PREFIX/opt/openblas/lib/pkgconfig:${PKG_CONFIG_PATH}"
export OWL_CFLAGS="-g -O3 -Ofast -funroll-loops -ffast-math -DSFMT_MEXP=19937 -fno-strict-aliasing -Wno-tautological-constant-out-of-range-compare"
export OWL_AEOS_CFLAGS="-g -O3 -Ofast -funroll-loops -ffast-math -DSFMT_MEXP=19937 -fno-strict-aliasing"
export EIGENCPP_OPTFLAGS="-Ofast -funroll-loops -ffast-math"
export EIGEN_FLAGS="-O3 -Ofast -funroll-loops -ffast-math"

# If opened a new terminal window, make sure to run `eval $(opam env)` first

opam install pyml toml lwt owl.0.10.0 ocamlformat.0.24.1 conf-openblas.0.2.1
```

You can now remove the `cc` symlink created earlier.

</TabItem>
</Tabs>

:::

## Other Installation Methods

There are various packaging options for Miking including `nix`, `guix`, and a
self-contained tarball. See the README under [misc/packaging](https://github.com/miking-lang/miking/tree/develop/misc/packaging)
directory in the Miking repository for more information.

We also provide Docker images for
[Miking on Docker Hub](https://hub.docker.com/r/mikinglang/miking) that come
with a prebuilt `mi` binary and all dependencies. See the documentation on
Docker Hub for how to run it.
