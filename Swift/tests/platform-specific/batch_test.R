require(Swift)

server <- commandArgs(TRUE)[[1]]


hostinfo1 <- getNodeList()
print("Auto:")
print(hostinfo1)

if (server == "cobalt") {
    hostinfo2 <- getNodeList("cobalt")
    print("cobalt:")
    print(hostinfo2)
}
if (server %in% c("pbs", "pbsf")) {
    hostinfo2 <- getNodeList("pbs")
    print("pbs:")
    print(hostinfo2)

    hostinfo3 <- getNodeList("pbsf")
    print("pbsf:")
    print(hostinfo3)
}
if (server == "sge") {
    hostinfo2 <- getNodeList("sge")
    print("sge:")
    print(hostinfo2)

    hostinfo3 <- getNodeList("pbsf")
    print("pbsf:")
    print(hostinfo3)
}

print ("Hosts:")
print(hostinfo1[[1]])

runAllSwiftTests(server="ssh", hosts=hostinfo1[[1]], cores=8)

