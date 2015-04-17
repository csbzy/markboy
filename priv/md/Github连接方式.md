Github https和ssh连接的区别
===================
有个项目在`push` 一直都需要输入帐号密码，发现原来是因为使用了https来连接；
使用https方式连接的话，`.git/config`里面的url配置是`https://github.com/XXX/XXX.git`,这样的话，每次push都需要输入帐号密码了。
而使用ssh方式连接的话，`.git/config`里面的url配置是`git@github.com:XXX/XXX.git`,这样的话，只要配置好了ssh key，就只需要输入`passphrase`就ok了。

至于如何设置连接方式?
可以通过`git remote set-url  origin`设置
``` shell
git remote set-url  origin git@github.com:XXX/XXX.git 
```
