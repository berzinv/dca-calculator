FROM swipl

COPY . /app
EXPOSE 8000

ENTRYPOINT ["swipl"]
CMD ["/app/app.prolog",  "--user=daemon", "--fork=false", "--port=8000"]
