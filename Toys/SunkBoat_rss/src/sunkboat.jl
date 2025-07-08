module sunkboat
export main
import HTTP
import XML

function (@main)(ARGS)
    r = HTTP.request("GET", "https://antirez.com/rss")
    if r.status != 200
        exit(1)
    end
    doc = XML.parse(String(r.body), XML.Node)
    ch = doc[1][1]

    for i in range(1, 10)
        item = ch[i]
        if (item.tag == "item")
            for t in item.children
                if (t.tag == "pubDate")
                    println("$(t.tag) -> $(t[1].value)")
                end
                if (t.tag == "link")
                    println("$(t.tag) -> $(t[1].value)")
                end
            end
        end
    end

end

end
