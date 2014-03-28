---
title: Using Remote Servers for Deep Learning, Part 1: Installing Python Packages Locally
date: 2014-03-27
author: Eliana
mathjax: on
---

Motivation
------------

I've recently become somewhat obsessed with neural nets. But training a convolutional net made normal computer activity tricky. Training two convolutional nets at once would make my laptop freeze up. And forget about training a net with Lin et. al.'s [Network in Network] architecture.

[Network in Network]: http://arxiv.org/abs/1312.4400


Finding More Computing Power
-------------

I don't attend university nor work for a major software company, so I don't easily have computer clusters at my disposal. Luckily, I have friends who attend university!

A friend gave me his login info for his university. Because I would be ssh'ing in a bunch, I edited my <code>.ssh/config</code> file[^1]:

[^1]: Stuff in caps is for privacy; replace with actual info.

```haskell
Host HOST
User USER
Port 22
HostName HOST.UNIVERSITY.edu
```

and ssh'ed in[^2]: ```ssh HOST```

[^2]: I couldn't use ssh with an authentication key instead of a password due to server configurations, but that is generally recommended.

Python was already installed, but my neural nets code needed [Theano]. Following the instructions for [easy installation]:

[Theano]: http://deeplearning.net/software/theano/
[easy installation]: http://deeplearning.net/software/theano/install_ubuntu.html

```bash
$ sudo apt-get install python-numpy python-scipy python-dev python-pip python-nose g++ libopenblas-dev git
[sudo] password for USER:
...
USER is not allowed to run sudo on HOST.  This incident will be reported.
```

Hwrm, right, I don't have sudo access. Sidenote: I'm sure many students messing around with unix have nearly had heart attacks from this message[^3].

[^3]: [Relevant xkcd.]

[Relevant xkcd.]:http://xkcd.com/838/



Installing Python Packages without Sudo Access
-----------------------------------------------

First, I needed Theano's dependencies ([NumPy] and [SciPy] -- luckily, the server already had [BLAS]), and a way to install them. Googling around [led me to] a [bunch] of [suggestions] that involved editing the `PATH`. This seemed overly complicated. 

[NumPy]: http://www.numpy.org/
[SciPy]: http://www.scipy.org/
[BLAS]: http://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms
[led me to]:http://stackoverflow.com/questions/622744/unable-to-install-python-without-sudo-access
[bunch]:http://www.astropython.org/tutorial/2010/1/User-rootsudo-free-installation-of-Python-modules
[suggestions]:http://askubuntu.com/questions/363300/how-to-install-pip-python-to-user-without-root-access

[Other] potential [solutions] suggested using [PEP] to install within the home folder: 

[other]:http://stackoverflow.com/questions/7465445/how-to-install-python-modules-without-root-access
[solutions]:http://stackoverflow.com/questions/7143077/use-pip-and-install-packages-at-my-home-folder
[PEP]:http://legacy.python.org/dev/peps/pep-0370/

```bash
# Try to install locally with pip
$ pip install --user package
The program 'pip' is currently not installed. You can install it by typing:
sudo apt-get install python-pip

# Try to install locally with easy_install
$ easy_install --user package
The program 'easy_install' is currently not installed. You can install it by typing:
sudo apt-get install python-setuptools
```

...For added fun, the server didn't have [pip] or [easy_install]. I was back where I started, needing sudo access. 

[pip]: https://pypi.python.org/pypi/pip
[easy_install]: https://pythonhosted.org/setuptools/easy_install.html

[Another article] explained how to download pip and install it locally[^4]. Then, I installed NumPy, SciPy, and Theano with pip. Other packages can also be installed this way. 

[Another article]: http://forcecarrier.wordpress.com/2013/07/26/installing-pip-virutalenv-in-sudo-free-way/

[^4]: This also works for setuptools.

```bash
# Download pip, and install it locally
wget https://raw.github.com/pypa/pip/master/contrib/get-pip.py
python get-pip.py --user
# Install packages with pip
pip install --user numpy
pip install --user scipy
pip install --user Theano
```

And that's it for installing Python packages locally, without sudo access!


Setting Up Neural Net Experiments
-----------------------------------

### Copying Necessary Directories

After I had all the libraries I needed, I needed to copy over the data and library:

```bash
scp -r neural_net_dir HOST:target/directory
```

Instead of just running experiments straight within the shell, it's better to run within screen so that experiments can be left running without a connection.

### Basics of Screen

To start a screen session, just enter ```screen``` and you'll be inside a screen session. To keep the session running in screen but detach from it so that it's running in the background, hit _ctrl-a,d_ (_ctrl_+_a_ at the same time, then _d_). To retach to that session from the shell, enter ```screen -rd```[^5]. This is also useful for keeping [irc] running all the time.

[irc]:http://en.wikipedia.org/wiki/Internet_Relay_Chat

[^5]: If you enter just ```screen```, you'll make a new screen session. 

### Running Experiments

All that was left was to actually run experiments within screen. Then I could detach the screen and close the ssh session, and go off and do whatever else I wanted (like write this blog post). To come back later, all I had to do was ssh and screen back in.


Future Plans
--------

Here are some things I'd like to eventually automate:

* Save the the neural net's setup and error rates to a file
* Save the neural net itself
* Make a graph of the train and test error rates over time
* Change the net hyperparameters and architecture, and run a whole bunch of tests at a time and in sequence