#!/bin/bash
cd /home/bottlenose/bottlenose
screen -S backburner -X quit 
screen -d -m -S backburner \
  env RAILS_ENV=production bundle exec rake backburner:work

