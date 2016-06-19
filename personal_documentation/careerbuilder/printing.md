Go to cbprinting/ in the browser at work (which seems to just redirect to
http://svrus4printlogic.cb.careerbuilder.com/). And search for you printer and
install it. Although I was only able to install one and after that it wouldn't
install more.

The command `lpstat` seems to give information about printers. For example
doing `lpstat -s` returns this for me:

```
system default destination: Green_Lantern
device for Black_Widow_0XQZ6UTRG: socket://10.30.31.81/
device for Green_Lantern: lpd://10.30.31.84/
device for SVR_LS_PRINT1_PLS_17THXEROX_7435: smb://svr-ls-print1.cb.careerbuilder.com/PLS-17THXEROX-7435
```

I also saw on a printer the name PLS-10THFLCENTRAL1 so I went to the
"Advanced" section of the Add Printers and Scanners section and entered
smb://PLS-10THFLCENTRAL1 into the URL. That seemed to add it without issue but
when I tried to print it it asked for my password. The weird thing though is
after adding that printer, other printers finally showed up in the "Default"
list of printers.
