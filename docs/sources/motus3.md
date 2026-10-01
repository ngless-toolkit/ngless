# How to integrate mOTUs3 with NGLess

This document describes how to integrate mOTUs3 with NGLess. You will need to replace `/your/folder/` with the path to the folder where you want to install mOTUs3.

## Step 0. Install NGLess

Install NGLess following the instructions at [https://ngless.readthedocs.io/en/latest/install.html](https://ngless.readthedocs.io/en/latest/install.html) into a conda environment called `ngless`.

## Step 1. Install NGLess-contrib

```bash
conda activate ngless

git clone https://github.com/ngless-toolkit/ngless-contrib.git
cd ngless-contrib
```


change the directory in `install.sh` in line 7 to a folder in (`/your/folder/Modules/` in the example, it is **very important** that the last directory is `Modules`)

## Step 2. Install mOTUs3

Run

```bash
./install.sh
cd /your/folder/Modules/motus.ngm/3.1
```

Run

```bash
python -m venv mOTUs-3.1.0-venv && \
    source mOTUs-3.1.0-venv/bin/activate && \
    wget https://github.com/motu-tool/mOTUs/archive/3.1.0.tar.gz && \
    tar xf 3.1.0.tar.gz && \
    rm -f 3.1.0.tar.gz && \
    pip install ./mOTUs-3.1.0 && \
    motus downloadDB && rm -rf mOTUs-3.1.0
```

## Step 3. Create an NGLess configuration file


Create an `ngless.conf` file with following lines (saved in `/your/folder/ngless.conf`):

```
user-directory = "/your/folder/"
user-data-directory = "/your/folder/"
temporary-directory= "/scratch/your_folder/temp/"
```

## Step 4. Import the module in your script

The mOTUs module is not one of the modules that ship with NGLess, so it must be
imported with `local import` (a plain `import "motus"` is an error, as it refers
to the obsolete mOTUs v1 module that is no longer supported):

```
ngless "1.6"
local import "motus" version "3.1"
```

NGLess looks for local modules in a `Modules` directory in the current
directory and in the `user-data-directory` (which is why it was set to
`/your/folder/` above).

## Step 5. Run NGLess

Run `ngless` with the option `--config-file /your/folder/ngless.conf` (for debugging, run `ngless` with the option `--trace`).

 
