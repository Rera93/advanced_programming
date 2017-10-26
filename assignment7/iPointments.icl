module iPointments 

import AUser, Appointment
import iTasks.Extensions.DateTime

Start :: *World -> *World
Start w = startEngine home w

home :: Task Appointment
home = enterInformation "New appointment" [] 