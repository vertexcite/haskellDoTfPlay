# Introduction
This shows how to build and run this little demo starting from a blank slate.

We use Nix and Stack, pretty much copying the approach and config from https://github.com/tensorflow/haskell (as at commit 99f8c8cf5658750b4ec9fe4bf10a37a80b9b62bd).

# Notes 

* Nix is used since not all of the dependencies are Haskell, and Nix is a good way of handling native (i.e. non-Haskell) dependencies.
* We could probably do the whole thing using just Nix, but I'm still getting my head around the Nix/Haskell method.  The use of Stack is based on the original approach used for this project, which relied on the user manually installing native dependencies.  Also, HaskellDo relies on stack, although these instructions are not targetted at using HaskellDo.
* Docker is not part of the requirements for reproducibility.  Rather, it is used here to give us a blank slate, in order to help ensure that all dependencies are specified in the code repository.

# Steps:
```
docker run --name haskellDoTfDemo -t -i ubuntu:16.04 /bin/bash
```
## Inside container
```
apt-get update
apt-get install curl bzip2 vim git libgmp-dev sudo
adduser user2
visudo
```
Add the following line via the editor
```
user2        ALL=(ALL)      ALL
```
Then save and exit
```
su - user2
curl https://nixos.org/nix/install | sh
```
Follow prompts
Once Nix installed, logout as user2, then log back in, i.e. 
```
CTRL-D
su - user2
nix-env -i stack

git clone https://github.com/vertexcite/haskellDoTfPlay.git
cd haskellDoTfPlay
# The next step takes a long time!
stack --nix build 
stack --nix exec run-test > demo.html 
```
(Note: during the above build, if out-of-memory errors occur, just rerun the command that was in progress, it usually resumes where it left off.  I'm assuming this is due to not giving the docker container enough memory.)

## Outside the container again
From another terminal, outside the docker container:
```
docker cp haskellDoTfDemo:/home/user2/haskellDoTfPlay/demo.html demo.html && open demo.html
docker kill haskellDoTfDemo
```
Expected result: browser opens showing example code for linear regression, with a plot of line fitted to the data points.
