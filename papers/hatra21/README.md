# HATRA 21 position paper

A position paper on Luau for [Human Aspects of Types and Reasoning Assistants](https://2021.splashcon.org/home/hatra-2021) (HATRA) 2021.

## Installing latexmk

First install  basictex
```
sudo brew install basictex
```

Then install the dependencies for the paper (sigh, by hand):

```
sudo tlmgr update --all
sudo tlmgr install acmart
sudo tlmgr install iftex
sudo tlmgr install xstring
sudo tlmgr install environ
sudo tlmgr install totpages
sudo tlmgr install trimspaces
sudo tlmgr install manyfoot
sudo tlmgr install ncctools
sudo tlmgr install comment
sudo tlmgr install balance
sudo tlmgr install preprint
sudo tlmgr install libertine
sudo tlmgr install inconsolata
sudo tlmgr install newtx
sudo tlmgr install latexmk
sudo tlmgr install montserrat
sudo tlmgr install ly1
```

## Building the paper

To build the paper:
```
latexmk --pdf hatra21
```

To run latexmk in watching mode (where it rebuilds the PDF on each change):
```
latexmk --pdf --pvc hatra21
```
