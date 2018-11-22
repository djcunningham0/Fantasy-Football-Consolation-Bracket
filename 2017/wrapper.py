from scrape import run
import time


minutes = 1

go = True
while go is True:
    run(google=1, sql=1, write_csv=1)  # available params are google, sql, write_csv

    # wait for 5 minutes before running again
    if minutes == 0:
        go = False
    else:
        print("\nWaiting...")
        time.sleep(60 * minutes - 30)
        print("30 seconds...")
        time.sleep(30)
