---
title: How to Install Python Packages Without Sudo Access 
date: 2014-03-24
author: Eliana
mathjax: on
---

Motivation
------------

I recently became somewhat obsessed with neural nets. But training a convolutional net made normal computer activity tricky. Training two convolutional nets at once would make my laptop freeze up. And forget about training a net with Lin et. al.'s [Network in Network] architecture.

[Network in Network]: http://arxiv.org/abs/1312.4400


Finding More Computing Power
-------------

I don't attend university nor work for a major software company, so I don't easily have computer clusters at my disposal. Luckily, I have friends who attend university!

A friend gave me his login info for his university. Because I would be ssh'ing in a bunch, I edited my <code>.ssh/config</code> file[^1]:

[^1]: stuff in caps is for privacy; replace with actual info

```javascript
Host HOSTNAME
User USERNAME
Port 22
HostName HOSTNAME.UNIVERSITY.edu
```

and ssh'ed in:

```bash
$ ssh HOSTNAME
```

Python was already installed, but my neural nets code needed [Theano] and its dependencies ([NumPy] and [SciPy] -- luckily, it already had [BLAS]!).

[Theano]: http://deeplearning.net/software/theano/
[NumPy]: http://www.numpy.org/
[SciPy]: http://www.scipy.org/
[BLAS]: http://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms


