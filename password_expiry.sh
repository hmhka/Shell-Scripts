
tee /usr/local/bin/pass.sh <<-'EOF'
#!/bin/bash
userlist="webas mqm oracle tomcat pnlse215 pnlac149 pnlma085 pnlak164 ihwa987 pnlrt168 pnlru022"
for r in $userlist
do
getent passwd $r > /dev/null 2&>1

if [ $? -eq 0 ]; then

chage -d $(date +%Y-%m-%d) $r
RHEL=$(uname -r | cut -d "." -f 6)
[ $RHEL == "el7" ] && pam_tally2 --user=$r  --reset || [ -f /usr/bin/faillog ] && /usr/bin/faillog -r $r || pam_tally2 --user=$r  --reset 

fi
done
EOF
chmod 774 /usr/local/bin/pass.sh
crontab -l|grep -i -v "pass.sh" > /tmp/crontab_root_before
cp -fp /tmp/crontab_root_before /var/spool/cron/crontab_root_before
echo "" >>/tmp/crontab_root_before
echo "#password non-expiry for functional account" >>/tmp/crontab_root_before
echo "0 0 1 * * /usr/local/bin/pass.sh >/dev/null 2>&1" >>/tmp/crontab_root_before
crontab -l |cat /tmp/crontab_root_before|uniq | crontab -
/usr/local/bin/pass.sh

================================

pam_tally2 --user=pnlse215
pam_tally2 --user=pnlse215  --reset

faillog -u pnlse215
/usr/bin/faillog -r pnlse215



=================================================
chage -M 90 ihwa987
chage -M 90 pnlrt168
chage -M 90 pnlru022

chage -l ihwa987
chage -l pnlrt168
chage -l pnlru022