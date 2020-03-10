import http.client
print("Learn UMA")

conn = http.client.HTTPSconnection("localhost", 18085)
conn.request("GET", "/")
r1 = conn.getresponse()
print(r1.status, r1.reason)
