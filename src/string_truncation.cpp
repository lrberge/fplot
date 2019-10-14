#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

const std::set<std::string> STOPWORDS{"i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "would", "should", "could", "ought", "cannot", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very"};

const std::set<std::string> GEOGRAPHIC{"ababa", "abuja", "accra", "addis", "afghanistan", "africa", "african", "aires", "alabama", "alaska", "albania", "albuquerque", "algeria", "algiers", "alofa", "amadora", "amman", "amsterdam", "anaheim", "anchorage", "andorra", "angeles", "angola", "ankara", "antananarivo", "antigua", "antonio", "antwerpen", "arabia", "argentina", "arhus", "arizona", "arkansas", "arlington", "armenia", "ashgabat", "asmara", "astana", "asuncion", "athens", "atlanta", "atoll", "aurora", "austin", "australia", "austria", "azerbaijan", "baghdad", "bahamas", "bahrain", "bakersfield", "baltimore", "bamako", "bandar", "bangkok", "bangladesh", "bangui", "banja", "banjul", "barbados", "barbuda", "barcelona", "basel", "basseterre", "baton", "beach", "begawan", "beijing", "beirut", "belarus", "belfast", "belgium", "belgrade", "belize", "belmopan", "benin", "bergen", "berlin", "bhutan", "birmingham", "bishkek", "bissau", "bloemfontein", "bogota", "boise", "bolivia", "bosnia", "boston", "botswana", "brasilia", "bratislava", "brazil", "brazzaville", "brest", "bridgetown", "bristol", "brunei", "brussels", "bucharest", "budapest", "buenos", "buffalo", "bulgaria", "burgas", "burkina", "burma", "burundi", "cairo", "california", "cambodia", "cameroon", "canada", "canberra", "capital", "caracas", "cardiff", "carolina", "castries", "cayenne", "central", "chandler", "charleroi", "charlotte", "chesapeake", "chicago", "chile", "china", "chisinau", "christi", "chula", "cincinnati", "cleveland", "coast", "colombia", "colombo", "colorado", "columbia", "columbus", "comoros", "conakry", "congo", "connecticut", "copenhagen", "corpus", "costa", "croatia", "cyprus", "czech", "czechia", "dakar", "dakota", "dallas", "damascus", "debrecen", "delaware", "delhi", "democratic", "denmark", "denver", "detroit", "dhabi", "dhaka", "diego", "district", "djamena", "djibouti", "dnipro", "dodoma", "domingo", "dominica", "dominican", "donaustadt", "donetsk", "dublin", "durham", "dushanbe", "ecuador", "edinburgh", "egypt", "eindhoven", "emirates", "england", "equatorial", "eritrea", "espoo", "essen", "estonia", "eswatini", "ethiopia", "favoriten", "federated", "finland", "florida", "floridsdorf", "france", "francisco", "frankfurt", "freetown", "fremont", "french", "fresno", "funafuti", "gabon", "gaborone", "gambia", "garland", "gdansk", "geneve", "genoa", "george", "georgetown", "georgia", "germany", "ghana", "gilbert", "gitega", "glendale", "goeteborg", "gomel", "greece", "greensboro", "grenada", "grenadines", "guatemala", "guiana", "guinea", "guyana", "hague", "haiti", "hamburg", "hampshire", "hanoi", "harare", "havana", "hawaii", "helsinki", "henderson", "herzegovina", "hialeah", "honduras", "honiara", "honolulu", "houston", "hrodna", "hungary", "iceland", "idaho", "illinois", "india", "indiana", "indianapolis", "indonesia", "ireland", "irvine", "irving", "islamabad", "island", "islands", "israel", "italy", "ivoire", "ivory", "jacksonville", "jakarta", "jamaica", "japan", "jersey", "jerusalem", "jordan", "kabul", "kampala", "kansas", "kathmandu", "kaunas", "kazakhstan", "kentucky", "kenya", "kharkiv", "khartoum", "kigali", "kingdom", "kingston", "kingstown", "kinshasa", "kiribati", "kitts", "klaipeda", "koeln", "korea", "kosice", "kosovo", "krakow", "kuala", "kuwait", "kyrgyzstan", "lanka", "laoghaire", "laredo", "latvia", "lebanon", "leone", "lesotho", "lexington", "liberia", "libreville", "libya", "liechtenstein", "liege", "lilongwe", "limited", "lincoln", "lisbon", "lithuania", "liverpool", "ljubljana", "london", "louis", "louisiana", "louisville", "luanda", "lubbock", "lucia", "lumpur", "lusaka", "luxembourg", "macedonia", "madagascar", "madison", "madrid", "mahilyow", "maine", "majuro", "malabo", "malaga", "malawi", "malaysia", "maldives", "malmoe", "malta", "managua", "manama", "manila", "maputo", "marino", "marseille", "marshall", "maryland", "maseru", "massachusetts", "mauritania", "mauritius", "mbabana", "melekeok", "memphis", "mexico", "miami", "michigan", "micronesia", "milan", "milwaukee", "minneapolis", "minnesota", "minsk", "miskolc", "mississippi", "missouri", "mogadishu", "moldova", "monaco", "mongolia", "monrovia", "montana", "montenegro", "montevideo", "moresby", "morocco", "moroni", "moscow", "mozambique", "munich", "muscat", "myanmar", "nairobi", "namibia", "nantes", "naples", "napoca", "nashville", "nassau", "nauru", "nebraska", "nepal", "netherlands", "nevada", "nevis", "newark", "niamey", "nicaragua", "nicosia", "niger", "nigeria", "nizhniy", "norfolk", "north", "northern", "norway", "nottingham", "nouakchott", "novgorod", "novosibirsk", "oakland", "odessa", "official", "oklahoma", "omaha", "oregon", "orlando", "orleans", "ostrava", "ottawa", "ouagadougou", "pakistan", "palau", "palermo", "palikir", "panama", "papua", "paraguay", "paramaribo", "paris", "patra", "pennsylvania", "petersburg", "philadelphia", "philippines", "phnom", "phoenix", "pilsen", "piraeus", "pittsburgh", "plano", "plovdiv", "podgorica", "poland", "portland", "porto", "portugal", "poznan", "prague", "praia", "pretoria", "prince", "principe", "pristina", "pyongyang", "qatar", "quito", "rabat", "raleigh", "recognition", "republic", "reykjavik", "rhode", "richmond", "riverside", "riyadh", "romania", "roseau", "rotterdam", "rouge", "russia", "rwanda", "sacramento", "saint", "salem", "salvador", "samara", "samoa", "santa", "santiago", "santo", "sarajevo", "saudi", "scotland", "scottsdale", "seattle", "senegal", "seoul", "serbia", "sevilla", "seychelles", "sheffield", "sierra", "singapore", "skopje", "slovakia", "slovenia", "sofia", "solomon", "somalia", "south", "spain", "split", "spokane", "springs", "states", "stockholm", "stockton", "sudan", "suriname", "swaziland", "sweden", "switzerland", "syria", "szeged", "taipei", "taiwan", "tajikistan", "tallinn", "tampa", "tampere", "tanzania", "tarawa", "tashkent", "tbilisi", "tegucigalpa", "tehran", "tennessee", "texas", "thailand", "thessaloniki", "thimphu", "tilburg", "timor", "tirana", "tirane", "tiraspol", "tobago", "tokyo", "toledo", "tonga", "toulouse", "trinidad", "tripoli", "tucson", "tulsa", "tunis", "tunisia", "turin", "turkey", "turkmenistan", "turku", "tuvalu", "uganda", "ukraine", "ulaanbaatar", "united", "uruguay", "utrecht", "uzbekistan", "vaduz", "valencia", "valletta", "vantaa", "vanuatu", "varna", "vatican", "vegas", "vella", "venezuela", "verde", "vermont", "victoria", "vienna", "vientiane", "vietnam", "vilnius", "vincent", "virginia", "vista", "vitebsk", "wales", "warsaw", "washington", "wayne", "wellington", "wichita", "windhoek", "winstona", "wisconsin", "worth", "wroclaw", "wyoming", "yamoussoukro", "yaounde", "yekaterinburg", "yemen", "yerevan", "zagreb", "zambia", "zaporizhia", "zaragoza", "zealand", "zenica", "zimbabwe", "zurich"};

std::string string_lower(std::string &s){
    // we just put it to lower

    std::string res = s;

    std::transform(res.begin(), res.end(), res.begin(),
                   [](unsigned char c){ return std::tolower(c); });

    return res;
}

std::string string_reconstruct(std::vector<std::string> &vs){
    // we just put it to lower

    int n = vs.size();
    std::string res = vs[0];

    for(int i=1 ; i<n ; ++i){
        res += ' ' + vs[i];
    }

    return res;
}

std::vector<std::string> string_split(std::string &s){
    // splits a character string along its spaces

    std::string data = s;

    std::vector<std::string> res;

    size_t pos = 0;
    std::string token;
    while ((pos = data.find(' ')) != std::string::npos) {
        token = data.substr(0, pos);

        res.push_back(token);

        data.erase(0, pos + 1);
    }

    res.push_back(data);

    return res;
}

int string_size(std::vector<std::string> x){
    // total size of a string vector (adding spaces)

    int res = 0;

    for(auto &s: x){
        res += s.size() + 1;
    }
    --res; // the last word has no space after it

    return res;
}

// [[Rcpp::export]]
std::string cpp_string_shorten(std::string x, int max_size = 15){
    // we shorten the string. Here is the plan:
    // 1) delete stopwords
    // 2) shorten non-geographic terms
    // 3) shorten geographic terms


    // if size OK, nothing
    if(int(x.size()) <= max_size + 1){
        return x;
    }

    std::vector<std::string> x_split_with_sw = string_split(x);
    std::vector<std::string> x_split;
    std::string res; // the result

    // 1) removing stopwords

    for(auto &s : x_split_with_sw){
        if(STOPWORDS.find(string_lower(s)) == STOPWORDS.end()){
            // NOT a stopword
            x_split.push_back(s);
        }
    }

    if(x_split.size() == 0){
        // Only stopwords => we just right trim
        res = x.substr(0, max_size - 2) + "..";
        return res;
    }

    int size_res = string_size(x_split);

    if(size_res <= max_size + 1){
        res = string_reconstruct(x_split);
        return res;
    }

    // If only one character string => becomes max_size
    if(x_split.size() == 1){
        res = x_split[0];
        res = res.substr(0, max_size - 2) + "..";
        return res;
    }


    // 2) Shortening non geographic names

    int size_s = 0;

    for(auto &s : x_split){
        if(GEOGRAPHIC.find(string_lower(s)) == GEOGRAPHIC.end()){
            // NOT a geographic name => we shorten it
            size_s = s.size();
            if(size_s > 5){
                s = s.substr(0, 4) + '.';
                size_res -= (size_s - 5);
                if(size_res <= max_size + 1){
                    // we stop as soon as possible
                    res = string_reconstruct(x_split);
                    return res;
                }
            }
        }
    }

    // 3) Shortening geographic names

    for(auto &s : x_split){
        size_s = s.size();
        if(size_s > 5){
            s = s.substr(0, 4) + '.';
            size_res -= (size_s - 5);
            if(size_res <= max_size + 1){
                // we stop as soon as possible
                res = string_reconstruct(x_split);
                return res;
            }
        }
    }

    // If still here... means still too large

    res = string_reconstruct(x_split);
    res = res.substr(0, max_size - 2) + "..";
    return res;
}










