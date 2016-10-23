-module(resolved_provider).

-export([get_zone/1]).

-include_lib("kernel/src/inet_dns.hrl").

get_zone(_Args) ->
    %% Fetch the Zone from a file, the DB, ...
    %% Here we just hardcode it
    {ok, [
        #dns_rr{domain="bot.co.za", type=soa, data={
            "ns1.bot.co.za",
            "hc.vst.io",
            870611,          %serial
            1800,            %refresh every 30 min
            300,             %retry every 5 min
            604800,          %expire after a week
            86400            %minimum of a day
            }
        },
        #dns_rr{domain="bot.co.za", type=ns, data="ns1.bot.co.za"},
        #dns_rr{domain="bot.co.za", type=ns, data="ns2.bot.co.za"},
        #dns_rr{domain="www.bot.co.za", type=cname, data="bot.co.za"},
        #dns_rr{domain="bot.co.za", type=a, data={127,0,0,1}},
        #dns_rr{domain="_udp._sip.bot.co.za", type=srv, data="sip1.bot.co.za"},
        #dns_rr{domain="_udp._sip.bot.co.za", type=srv, data="sip2.bot.co.za"},
        #dns_rr{domain="sean.sean", type=soa, data={
            "ns1.sean.sean",
            "hc.sean.sean",
            870611,          %serial
            1800,            %refresh every 30 min
            300,             %retry every 5 min
            604800,          %expire after a week
            86400            %minimum of a day
            }
        },
        #dns_rr{domain="sean.sean", type=ns, data="ns1.bot.co.za"},
        #dns_rr{domain="sean.sean", type=ns, data="ns2.bot.co.za"},
        #dns_rr{domain="www.sean.sean", type=cname, data="bot.co.za"},
        #dns_rr{domain="sean.sean", type=a, data={127,0,0,1}},
        #dns_rr{domain="_udp._sip.sean.sean", type=srv, data="sip1.z1.sean.sean"},
        #dns_rr{domain="_udp._sip.sean.sean", type=srv, data="sip.z1.sean.sean"}
    ]}.
