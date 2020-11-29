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

When I was using ubuntu at another company I just did:
1. System settings
2. Printers
3. Add
4. Entered the "printer name" which for me was IL-320Ohio-P5 and then
   the printer showed up! Without entering the address NO printers
   showed up for me.

Woa, so I think those printer names really are local DNS names because
I was able to go to http://il-320ohio-p5:631 in my browser and see
stuff.

When adding this printer to my machine I had to install a driver as
well. I found the name of the model on the machine (it was also on the
above url) of XM5163. I scrolled through the list of printer drivers,
went to the Lexmark section (which was the printer brand I guess), and
then I found XM5163!
