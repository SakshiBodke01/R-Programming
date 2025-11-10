# Load libraries
library(ggplot2)
library(dplyr)
library(jsonlite)

# Load dataset
games <- read.csv("vgsales.csv")

#  VIDEO GAME SALES ANALYSIS 
cat("=== VIDEO GAME SALES ANALYSIS ===\n\n")

# Basic stats
total_games <- nrow(games)
total_sales <- sum(games$Global_Sales)
top_genre <- names(sort(table(games$Genre), decreasing = TRUE)[1])
peak_year <- names(sort(table(games$Year), decreasing = TRUE)[1])

cat("Total Games:", total_games, "\n")
cat("Total Sales:", round(total_sales), "million\n")
cat("Most Popular Genre:", top_genre, "\n")
cat("Peak Year:", peak_year, "\n\n")

#  Data for Charts 
genre_sales <- games %>%
  group_by(Genre) %>%
  summarise(Sales = sum(Global_Sales)) %>%
  arrange(desc(Sales)) %>%
  head(8)

regional_sales <- c(
  sum(games$NA_Sales),
  sum(games$EU_Sales),
  sum(games$JP_Sales),
  sum(games$Other_Sales)
)

platform_sales <- games %>%
  group_by(Platform) %>%
  summarise(Sales = sum(Global_Sales)) %>%
  arrange(desc(Sales)) %>%
  head(10)

top_publishers <- games %>%
  group_by(Publisher) %>%
  summarise(Sales = sum(Global_Sales)) %>%
  arrange(desc(Sales)) %>%
  head(6)

top_games <- games %>%
  arrange(desc(Global_Sales)) %>%
  head(6)

#  HTML Dashboard 
html_content <- sprintf('
<!DOCTYPE html><html><head><meta charset="UTF-8"><title> Video Game Sales</title>
<script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
<style>
body{font-family:Arial;background:linear-gradient(135deg,#667eea,#764ba2);color:#fff;margin:20px;text-align:center}
h1{margin:10px 0}
.grid{display:grid;grid-template-columns:repeat(4,1fr);gap:10px;margin:10px}
.card{background:#fff;color:#333;padding:15px;border-radius:10px;box-shadow:0 4px 8px rgba(0,0,0,.2)}
.chart{background:#fff;border-radius:10px;box-shadow:0 4px 8px rgba(0,0,0,.2);margin:10px auto;padding:15px;max-width:600px}
canvas{height:320px!important}
</style></head><body>
<h1>Video Game Sales Dashboard</h1>
<div class="grid">
<div class="card"><h3>%s</h3><p>Total Games</p></div>
<div class="card"><h3>%sM</h3><p>Total Sales</p></div>
<div class="card"><h3>%s</h3><p>Top Genre</p></div>
<div class="card"><h3>%s</h3><p>Peak Year</p></div>
</div>
<div class="chart"><canvas id="genre"></canvas></div>
<div class="chart"><canvas id="region"></canvas></div>
<div class="chart"><canvas id="platform"></canvas></div>
<div class="chart"><canvas id="publisher"></canvas></div>
<div class="chart"><canvas id="games"></canvas></div>
<script>
new Chart(genre,{type:"bar",data:{labels:%s,datasets:[{label:"Sales (M)",data:%s,backgroundColor:"rgba(102,126,234,.8)"}]},options:{scales:{y:{beginAtZero:true}}}});
new Chart(region,{type:"doughnut",data:{labels:["NA","EU","JP","Other"],datasets:[{data:%s,backgroundColor:["#ff6384","#36a2eb","#ffce56","#4bc0c0"]}]}});
new Chart(platform,{type:"bar",data:{labels:%s,datasets:[{label:"Sales (M)",data:%s,backgroundColor:"rgba(118,75,162,.8)"}]},options:{indexAxis:"y",scales:{x:{beginAtZero:true}}}});
new Chart(publisher,{type:"bar",data:{labels:%s,datasets:[{label:"Sales (M)",data:%s,backgroundColor:"rgba(255,159,64,.8)"}]},options:{scales:{y:{beginAtZero:true}}}});
new Chart(games,{type:"bar",data:{labels:%s,datasets:[{label:"Sales (M)",data:%s,backgroundColor:["#ff6384","#36a2eb","#ffce56","#4bc0c0","#9966ff","#ff9f40"]}]},options:{indexAxis:"y",scales:{x:{beginAtZero:true}}}});
</script></body></html>',
                        format(total_games,big.mark=","),round(total_sales),top_genre,peak_year,
                        toJSON(genre_sales$Genre),toJSON(round(genre_sales$Sales)),
                        toJSON(round(regional_sales)),
                        toJSON(platform_sales$Platform),toJSON(round(platform_sales$Sales)),
                        toJSON(top_publishers$Publisher),toJSON(round(top_publishers$Sales)),
                        toJSON(top_games$Name),toJSON(round(top_games$Global_Sales,2))
)

# Save and Open
writeLines(html_content,"game_sales_dashboard.html")
browseURL("game_sales_dashboard.html")
cat("✓ Dashboard created and opened successfully!\n")
