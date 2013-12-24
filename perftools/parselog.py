import re
import itertools as its
import datetime
from operator import itemgetter


def runlength_enc(xs):
    '''Return a run-length encoded version of the stream, xs.
    
    The resulting stream consists of (count, x) pairs.
    
    >>> ys = runlength_enc('AAABBCCC')
    >>> next(ys)
    (3, 'A')
    >>> list(ys)
    [(2, 'B'), (3, 'C')]
    '''
    return ((ilen(gp), x) for x, gp in its.groupby(xs))

def ilen(it):
    '''Return the length of an iterable.
    
    >>> ilen(range(7))
    7
    '''
    return sum(1 for _ in it)


fname = "rserver2.log"
    

pattern = (r"^([-\d]+)" + r"\s+"
         + r"([:\d]+),([\d]+)\+([\d]+)" + r"\s+"
         + r"([\S]+)" + r"\s+" + r"([\S]+)"
         + r"(.+)$" )


compiled = re.compile(pattern)

time_rex = r"(\d\d):(\d\d):(\d\d)"
def parse_time(timestr):
    m = re.match(time_rex, timestr)
    return int(m.group(1)), int(m.group(2)), int(m.group(3))
    
date_rex = r"(\d\d\d\d)-(\d\d)-(\d\d)"
def parse_date(datestr):
    m = re.match(date_rex, datestr)
    if m is None:
        print datestr
    return int(m.group(1)), int(m.group(2)), int(m.group(3))

def log_iter(filter_type=None, message_rex=None):   
    for line in open(fname).readlines():
        matched = re.match(compiled, line)
        if matched is None:
            #print "Error on line" + line
            pass
        else:
            (date, time, milliseconds, something, priority,
                    type, message) = matched.groups()
            if filter_type is not None and filter_type != type:
                continue
            if message_rex is not None and re.match(message_rex, message) is None:
                #print message
                continue
            yield (date, time, milliseconds, something, priority,
                    type, message)


def time_converted(recs, basetime=None):
    for (date, time, milliseconds, something, priority,
                    type, message) in recs:

        year, month, day = parse_date(date)
        h, m, s = parse_time(time)
        t = datetime.datetime(year, month, day, h, m, s, int(milliseconds) * 1000)
        if basetime is not None:
            if t < basetime:
                continue
            td = t - basetime
            yield (td, priority, type, message)
        else:
            yield(t, priority, type, message)


def get_cmd_times(filter_type, message_rex=None):
    return list(runlength_enc((date, time, type)
                for date, time, milliseconds, something, priority,
                    type, message in log_iter(filter_type, message_rex)))



def handle(count, date, time, type, message=None):
    if message is not None:
        print message,
    print count, date, time, type


#for count, tup in get_cmd_times("PutFileCommand"):
#    handle(count, *tup)

#for count, tup in get_cmd_times("GridExec"):
#    handle(count, *tup)

#for count, tup in get_cmd_times("Cpu", re.compile("^.*jobTerminated.*$")):
#    handle(count, tup[0], tup[1], tup[2], message="jobTerminated")


exec_events = log_iter("vdl:execute")


#basetime = datetime.datetime(2011, 3, 18, 13, 22, 52, 231*1000)
basetime = datetime.datetime(2011, 3, 28, 19, 39, 25, 707*1000)
exec_events = list(time_converted(exec_events, basetime))

starts = [(t, priority, type, message, message.split()[1])
            for t, priority, type, message in exec_events 
            if message.find("START") >= 0]
ends = [(t, priority, type, message, message.split()[1])
        for t, priority, type, message in exec_events 
        if message.find("END_SUCCESS") >= 0]

starts.sort(key=itemgetter(4))
ends.sort(key=itemgetter(4))
for s, e in zip(starts[:10], ends[:10]):
    print s, e

def to_s(td):
    return td.days * 24 * 60 * 60 + float(td.seconds) + float(td.microseconds) / 1000000.0

paired = [(s[0], e[0], to_s(e[0]) - to_s(s[0])) 
        for s,e in zip(starts, ends)
        if to_s(s[0]) > 20600]
        #if to_s(s[0]) > 300]
paired.sort(key=itemgetter(2))
paired.reverse()
paired = [(i, s, e, d ) for i, (s,e,d) in zip(range(len(paired)), paired)]
print paired
import pylab
for i, s, e, diff in paired:
    pylab.plot((to_s(s), to_s(e)), (i, i), 'r')

pylab.xlabel("time (s)")
pylab.ylabel("number of active bootstrap tasks.")

pylab.show()

pylab.hist([t[3] for t in paired], bins=20)
pylab.xlabel("time (s)")
pylab.ylabel("# tasks")
pylab.show()
