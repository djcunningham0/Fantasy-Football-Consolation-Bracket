# Helpful link for writing to Google sheets:
# https://www.twilio.com/blog/2017/02/an-easy-way-to-read-and-write-to-a-google-spreadsheet-in-python.html
#
# gspread documentation
# http://gspread.readthedocs.io/en/latest/

from urllib.request import urlopen
from bs4 import BeautifulSoup
import csv
from datetime import datetime
import pymysql
import gspread as gs
from oauth2client.service_account import ServiceAccountCredentials


def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False


def run(google=1, sql=0, write_csv=0):
    ############
    week = 16  # update this
    ############

    data = []

    managers = ['Danny', 'Mike S', 'Will', 'Sean', 'Siyang', 'Kate', 'Claire', 'Mike Y',
                'Ben', 'Amelia', 'Sierra', 'Griffin', 'Bennett', 'Eliot']

    pageUrls = []
    for i in range(1, 15):
        pageUrls.append('https://football.fantasysports.yahoo.com/f1/1120510/'+str(i)+'/team?week='+str(week))

    for n in range(len(pageUrls)):
        manager = managers[n]

        pg = pageUrls[n]
        page = urlopen(pg)
        soup = BeautifulSoup(page, 'html.parser')

        nameBox = soup.find('a', attrs={'class': 'Navtarget Py-sm Pstart-lg F-reset Wordwrap-bw No-case'})
        teamName = nameBox.text.strip()  # get the text and strip leading and trailing spaces
        teamName = teamName[0:len(teamName)-3]  # remove the weird non-printable character

        print(teamName, '...')

        positions = soup.find_all('span', attrs={'class': 'pos-label'})
        # add counters to positions with multiple starters
        wr, rb, bn = 1, 1, 1
        for i in range(len(positions)):
            if positions[i].text == 'WR':
                positions[i] = 'WR'+str(wr)
                wr += 1
            elif positions[i].text == 'RB':
                positions[i] = 'RB' + str(rb)
                rb += 1
            elif positions[i].text == 'BN':
                positions[i] = 'BN' + str(bn)
                bn += 1
            else:
                positions[i] = positions[i].text

        points = soup.find_all('td', attrs={'class': 'Alt Ta-end Nowrap Bdrstart'})  # points from offensive players
        points2 = soup.find_all('td', attrs={'class': 'Ta-end Nowrap Bdrstart'})     # points from kicker and defense
        for i in range(len(points2)):
            points.append(points2[i])  # put all points in the same array

        players = soup.find_all('a', attrs={'class': 'Nowrap name F-link'})

        # fill the data frame with the points for each roster
        for i in range(len(players)):
            pts = points[i].text
            if not is_number(pts):
                pts = ""  # if player hasn't played yet, points = "–". convert it to blank
            pos = positions[i]
            team_pos = teamName + "_" + pos
            data.append((week, manager, teamName, pos, team_pos, players[i].text, pts))

    # write to MySQL database
    if sql == 1:
        print("\nWriting to database...")
        conn = pymysql.connect(host='localhost',
                               user='root',
                               password='',
                               port=3306,
                               db='FFB')

        # delete rows previously entered for this week
        deleteRows = "Delete from Consolation2017 where Week = " + str(week)
        try:
            with conn.cursor() as cursor:
                cursor.execute(deleteRows)
        except BaseException as e:
            print("Error: " + str(e))

        # write data frame into database
        command = "Insert into `Consolation2017` (`Week`, `Manager`, `Team_Name`, `Position`, `Team_Pos`, " \
                  "`Player`, `Points`, `Last_Update`) " \
                  "Values (%s, %s, %s, %s, %s, %s, %s, %s)"

        for week, manager, team, pos, team_pos, player, points in data:
            try:
                with conn.cursor() as cursor:
                    if points == '':
                        points = None
                    cursor.execute(command, (week, manager, team, pos, team_pos, player, points, datetime.now()))
                conn.commit()
            except BaseException as e:
                print('Error: ' + str(e))

        conn.close()
        print("Database complete.")

    # write to CSV file
    if write_csv == 1:
        print("\nWriting CSV...")
        with open('./week'+str(week)+'_data.csv', mode='w') as csvFile:
            writer = csv.writer(csvFile)
            writer.writerow(['manager', 'team name', 'position', 'team_pos', 'player', 'points', 'last update time'])
            for week, manager, team, pos, team_pos, player, points in data:
                writer.writerow([manager, team, pos, team_pos, player, points, datetime.now()])
        print("CSV complete.")

    # write directly to Google spreadsheet
    if google == 1:
        print("\nUpdating Google sheet...")

        scope = ['https://spreadsheets.google.com/feeds',
                 'https://www.googleapis.com/auth/drive']
        creds = ServiceAccountCredentials.from_json_keyfile_name('client_secret.json', scope)
        client = gs.authorize(creds)

        # open the Google spreadsheet
        print("Opening spreadsheet...")
        try:
            sheet = client.open("Consolation bracket")
        except BaseException as e:
            print('Error opening spreadsheet: ' + str(e))

        # select or create the current week's data sheet
        name = "week " + str(week) + " data"
        print("Opening " + name + "...")
        try:
            sheet = sheet.worksheet(name)
        except BaseException as e:
            try:
                print("Creating sheet: " + name)
                sheet.add_worksheet(name, 300, 10)
                sheet = sheet.worksheet(name)
            except BaseException as e:
                print("Error creating sheet: " + str(e))

        # add headers
        headers = ['manager', 'team name', 'position', 'team_pos', 'player', 'points', 'last update time']
        for i in range(len(headers)):
            try:
                if sheet.cell(1, i + 1).value != headers[i]:
                    sheet.update_cell(1, i + 1, headers[i])
                # sheet.update_cell(1, 1, "manager")
                # sheet.update_cell(1, 2, "team name")
                # sheet.update_cell(1, 3, "position")
                # sheet.update_cell(1, 4, "team_pos")
                # sheet.update_cell(1, 5, "player")
                # sheet.update_cell(1, 6, "points")
                # sheet.update_cell(1, 7, "last update time")
            except BaseException as e:
                print('Error adding headers:' + str(e))

        # update data
        print("Updating data...")
        row = 2
        for week, manager, team, pos, team_pos, player, points in data:
            if row % 10 == 0:
                print("Row", row)

            # update the first five columns
            vals = [manager, team, pos, team_pos, player]
            for i in range(len(vals)):
                try:
                    # writing is much slower than reading, so check value first
                    if str(sheet.cell(row, i + 1).value) != str(vals[i]):
                        sheet.update_cell(row, i + 1, vals[i])
                        # print("update: " + headers[i] + " (" + vals[i] + ")")
                except BaseException as e:
                    print("Error updating cell: " + str(e))
                    print("Row", row, "(value: " + headers[i] + ")")

            # points is handled differently b/c it's a float not a string
            try:
                cell_val = sheet.cell(row, 6).value
                if cell_val != "" and points != "" and float(cell_val) != float(points):
                    sheet.update_cell(row, 6, points)
                elif str(cell_val) + str(points) != "":    # then one of them isn't null
                    sheet.update_cell(row, 6, points)
                sheet.update_cell(row, 7, datetime.now())  # always update datetime
            except BaseException as e:
                print("Error updating cell: " + str(e))
                print("Row", row, "(" + team_pos + ")")
            row += 1

        # clear out some extra rows in case the number of owned players changed
        for row in range(row, 14 * 17 + 1):
            for col in range(7):
                if sheet.cell(row, col + 1).value != "":
                    try:
                        sheet.update_cell(row, col + 1, "")
                    except BaseException as e:
                        print("Error clearing cell: " + str(e))

        print("\nGoogle sheet update complete.")

    print("Done.")
