package main

import (
	"fmt"
	"log"
	"net/http"

	"github.com/PuerkitoBio/goquery"
)

func main() {
	blogTitles, err := GetLatestBlogTitles("https://golangcode.com")
	if err != nil {
		log.Println(err)
	}
	fmt.Println("Blog Titles:")
	fmt.Printf(blogTitles)
}
