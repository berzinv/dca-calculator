FROM swipl

COPY . /app
EXPOSE 8080

ENTRYPOINT ["swipl"]
CMD ["/app/app.prolog",  "--user=daemon", "--fork=false", "--port=8080"]
