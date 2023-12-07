package main

import (
	"database/sql"
	"encoding/csv"
	"flag"
	"fmt"
	_ "github.com/mattn/go-sqlite3"
	"io"
	"log"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	csvPath := flag.String("csv", "./oscar_age_female.csv", "path to csv file")
	dbPath := flag.String("o", "./oscar.db", "desired path for output database")
	flag.Parse()
	println("Reading csv ->", *csvPath)
	println("Storing to ->", *dbPath)
	csvFile, err := os.Open(*csvPath)
	if err != nil {
		log.Fatal(err)
	}

	csvReader := csv.NewReader(csvFile)
	csvReader.LazyQuotes = true
	csvReader.Comma = ','
	csvReader.TrimLeadingSpace = true
	head, err := csvReader.Read()
	if err != nil {
		panic(err)
	}

	csvReader.FieldsPerRecord = 5

	headFields := ""
	for i := 0; i < len(head)-1; i++ {
		headFields += "`" + head[i] + "` TEXT, "
	}
	headFields += head[len(head)-1] + " TEXT"

	dbname := filepath.Base(*dbPath)
	dbname = strings.TrimSuffix(dbname, filepath.Ext(dbname))

	database, _ :=
		sql.Open("sqlite3", *dbPath)

	statement, _ := database.Prepare(fmt.Sprint("DROP TABLE IF EXISTS `", dbname, "`;"))
	_, err = statement.Exec()

	statement, _ = database.Prepare("CREATE TABLE IF NOT EXISTS `" + dbname + "` (" + headFields + ");")
	_, err = statement.Exec()
	if err != nil {
		println(err)
	}

	quest := make([]string, len(head))
	for i := range quest {
		quest[i] = "?"
	}

	for {
		line, err := csvReader.Read()
		if err == io.EOF {
			break
		} else if err != nil {
			log.Fatal(err)
		}

		var qur = fmt.Sprintf("INSERT INTO %s (\"%s\") VALUES ('%s');", dbname, strings.Join(head, "\",\""), strings.Join(line, "','"))
		println(qur)
		_, err = database.Exec(qur)
		if err != nil {
			return
		}
	}

	rows, _ :=
		database.Query("SELECT* FROM " + dbname)
	fmt.Println(rows)

	for rows.Next() {
		fmt.Println(rows)
	}
}
