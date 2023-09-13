To generate the handin-client package as a zip file, you need to follow the following steps (also see the [`README.md` in the handin-config repo](https://github.com/ps-tuebingen/handin-config/blob/master/README.md) for more details on how to test it).

## Preparation: Checkout sources (once)

You first need to checkout/update the sources.
Checkout instructions are below; you can update with `git pull` and variants as needed.

1.  Choose which variant of the `handin` package to use.
    You could in principle use the official variant, https://github.com/plt/handin; but in this document, we'll assume you want to use our variant: https://github.com/ps-tuebingen/handin, which contains various changes and this tool.

2.  Checkout the handin package in some directory of your choice:

    ```sh
    git clone https://github.com/ps-tuebingen/handin.git
    PATH_TO_HANDIN=$PWD/handin
    ```

    After this, `$PATH_TO_HANDIN` will be the path to the created repo.

3.  Create your `handin-config` directory --- you will probably want to use ours:

    ```sh
    git clone https://github.com/ps-tuebingen/handin-config.git
    ```

    That repo contains complete configs for our Tübingen course; make sure to checkout the branch you want among ones named deploy-* (or master for the local version).
    If instead you are an external user, setup `config.rktd` according to docs on [server setup][1].
    Also include the correct `server-cert.pem`.
    On the server (but not on the client) you will also need `private-key.pem`, which you should never commit to a public Git repo!

## Install handin package

To assemble packages, you need first to add `handin` to the package database.
This needs to be repeated when updating your Racket version, but not when the code changes since this step adds a link to the checkout, so changes are integrated automatically.

1.  With `$PATH_TO_HANDIN` setup as before, install the handin package:
    ```sh
    raco pkg install -n handin $PATH_TO_HANDIN
    ```

2.  If Racket complains that `handin` is already installed, remove it with `raco pkg remove --no-trash handin`.

## Generate plugin packages for distribution

1.  Go to your `handin-config` directory, where you have the configuration file `config.rktd`:

    ```sh
    cd handin-config
    ```

2.  Run this tool with:
    ```sh
    racket -l generate-handin-client
    ```

This will produce a zip file and its checksum, for clients to install.
The name will be the `client-name` from `config.rktd`.

You don't need to adjust `${PATH_TO_HANDIN}/handin-client/info.rkt`.
It will be regenerated with the information from `config.rktd`.

## Generating plugin directory for local use (no zip)

1.  Go to your `handin-config` directory:
    ```sh
    cd handin-config
    ```

2.  Run this tool with:
    ```sh
    racket -l generate-handin-client -- --local
    ```

    This will create a `utue-info1-ws??-local` folder.

3.  For the first install, expose this folder to Racket via:

    ```sh
    raco pkg install ./utue-info1-ws??-local
    ```

    Later, if wanted, you *can* but *needn't* update compiled code and docs via:

    ```sh
    raco pkg update ./utue-info1-ws??-local
    ```

[1]: http://pkg-build.racket-lang.org/doc/handin-server/server-setup.html
