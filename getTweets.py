import urllib2 as urllib
import json
import oauth2 as oauth
import re
import time
from optparse import OptionParser

def credentials():

    access_token_key = "2407435232-QXOkFRVn2dDTlQ1fn0lMoeno8J4dw4qtqSnaECt"
    access_token_secret = "vP1CJEJMluZGRRJrmeetmDuf7fIRwPDxyWj7FunjMljK4"

    consumer_key = "S5Y6XF6fXT6DUULsdd5Cg"
    consumer_secret = "lG4tQLeHQlxXGjQiPUrGtnV0MYSALN7cEHGWwlyo6LE"

    oauth_token    = oauth.Token(key=access_token_key,secret=access_token_secret)
    oauth_consumer = oauth.Consumer(key=consumer_key,secret=consumer_secret)

    return (oauth_consumer,oauth_token)


def twitterreq(url, oauth_consumer, oauth_token, method, parameters):

    req = oauth.Request.from_consumer_and_token(oauth_consumer,
                                                token=oauth_token,
                                                http_method="GET",
                                                http_url=url,
                                                parameters=parameters)

    signature_method_hmac_sha1 = oauth.SignatureMethod_HMAC_SHA1()
    req.sign_request(signature_method_hmac_sha1, oauth_consumer, oauth_token)

    encoded_post_data = None
    url = req.to_url()

    http_handler  = urllib.HTTPHandler(debuglevel=0)
    https_handler = urllib.HTTPSHandler(debuglevel=0)

    opener = urllib.OpenerDirector()
    opener.add_handler(http_handler)
    opener.add_handler(https_handler)

    response = opener.open(url, encoded_post_data)

    return response


class emojiDecoder:

    emoji_list = {}

    myre = re.compile(u'['
         u'\U0001F300-\U0001F64F'
         u'\U0001F680-\U0001F6FF'
         u'\u2600-\u26FF\u2700-\u27BF'
         u'\U000FE4EC]+', 
         re.UNICODE)

    def __init__(self):

        emoji_dict = open('emoji.json','r')
        emoji_json = json.loads(emoji_dict.readline())

        for emo in emoji_json.items():
            self.emoji_list[ emo[1][0] ] = ":"+emo[1][1][0]+":"

    def emoDecode(self,text):

      # decode emojis
      for c in text:
         if c in self.emoji_list :
            text = re.sub(c, self.emoji_list[c], text)

      # purge remaining unknown emojis
      text = self.myre.sub(':emoji:',text) #.encode('utf-8')

      return text

ed = emojiDecoder()

def printTweet(line):

    if len(line) == 0:
        return

    tweet_json = json.loads(line)

    # only accept records with a 'user' field
    if tweet_json.get('user'):
        postId   = tweet_json['id']
        userId   = tweet_json['user']['id']
        userName = tweet_json['user']['name'] #.encode('utf8')
        location = tweet_json['user']['location'] #.encode('utf8')
        time_at  = re.sub(r'\+0000 ','', tweet_json['created_at'] )
        text     = tweet_json['text']
        text     = ed.emoDecode( re.sub(r'"','``', text ) )
        post_time = time.strptime(time_at)
        geoCoord = []
        if tweet_json['geo'] :
            geoCoord.append( tweet_json['geo']['coordinates'][1] )
            geoCoord.append( tweet_json['geo']['coordinates'][0] )

        coordType = 0
        if tweet_json.get('place'):
            placeId = tweet_json['place']['id']

            if len(geoCoord) == 0 and tweet_json['place'].get('bounding_box') and tweet_json['place']['bounding_box'].get('coordinates'):
                coordType = 1
                poly = tweet_json['place']['bounding_box']['coordinates']
                geoCoord.append( (poly[0][0][0] + poly[0][2][0])/2. )
                geoCoord.append( (poly[0][0][1] + poly[0][1][1])/2. )
            else :
                geoCoord.append( 0 )
                geoCoord.append( 0 )
        else :
            placeId  = 0
            geoCoord.append( 0 )
            geoCoord.append( 0 )

        place = placeName(geoCoord)

        print "{0},{1},{2},{3},{4},{5},{6},{7},{8},\"{9}\"".format(postId,userId,placeId,geoCoord[0],geoCoord[1],coordType,place,len(text),time.strftime('%Y-%m-%d %H:%M:%S',post_time),text.encode('utf8'));

geo = {
     'c': ("California",   [[-126,32],   [-114,42]]),
     'y': ("NY",           [[-74,40],    [-73,41]]),
     'l': ("London",       [[-1,48.6],   [1,49.]]),
     'p': ("Pairs",        [[2,51.1],    [2.51,51.7]]),
     'm': ("Moskow",       [[37.4,55.5], [37.9,55.9]]),
     's': ("St.Peterburg", [[30.0,59.5], [30.7,60.4]]),
     'k': ("Kiev",         [[30.4,50.3], [30.8,50.6]]),
     'u': ("Ukraine",      [[22.1,44.3], [41.5,52.1]]),
     'e': ("Ekatirinburg", [[60.3,56.6], [60.9,57.0]]),
     'n': ("Novosibirsk",  [[82.8,54.1], [83.1,54.8]]),
     'v': ("Vladivostok",  [[131.5,42.9],[132.3,43.4]])
}

def placeName(geoCoord):

    for (name,coord) in geo.values():
        if  geoCoord[0] >= coord[0][0] and geoCoord[0] <= coord[1][0] and geoCoord[1] >= coord[0][1] and geoCoord[1] <= coord[1][1] :
            return name

    return "unknown"

placeCache = {}

def queryPlace(placeId):

    geoCoord = [0,0]

    if placeId in placeCache:
        geoCoord = placeCache[placeId]

    else:

        (oauth_consumer,oauth_token) = credentials()

        url = "https://api.twitter.com/1.1/geo/id/" + placeId + ".json"

        parameters = []
        response = twitterreq(url, oauth_consumer, oauth_token, "GET", parameters)

        for line in response:
            print line
            place_json = json.loads(line)
            if place_json.get('centroid'):
                geoCoord = place_json['centroid']

        placeCache[placeId] = geoCoord

    return geoCoord


def fetchsamples(locations):

    if locations == "a":
        locations = "".join(geo.keys())

    url = "https://stream.twitter.com/1.1/statuses/filter.json?"

    nLoc = 0
    for loc in locations:
        if loc not in geo.keys():
            print "Location error: ",loc
            return
        else:
            if nLoc != 0:
                url = url + '&'
            url = url + 'locations='+str(geo[loc][1][0][0])+","+str(geo[loc][1][0][1])+","+str(geo[loc][1][1][0])+","+str(geo[loc][1][1][1])
            nLoc += 1

    (oauth_consumer,oauth_token) = credentials()

    parameters = []
    response = twitterreq(url, oauth_consumer, oauth_token, "GET", parameters)
    for line in response:
        printTweet( line.strip() )

#####################################################################

parser = OptionParser()
parser.add_option("-l", "--loc", dest="location", help="Kiev/Moscow/St.Peterburg/Ekatirinburg/Novosibirsk/Vladivostok")
options, arguments = parser.parse_args()

print "postId," + "userId," + "placeId," + "Long," + "Lat," + "coordType," + "PlaceName," + "PostLength," + "DateTime," + "Text";

fetchsamples(options.location)

#queryPlace("25530ba03b7d90c6")
