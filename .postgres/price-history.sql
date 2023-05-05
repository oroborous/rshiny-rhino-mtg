-- data for price history page
create or replace view v_price_history as
select b.useremail as grouptype, b.setcode, b.setname, b.releasedate, b.type as cardtype, b.color,
	c.pricedate, sum(c.avgretailprice) as avgretailprice, sum(c.avgbuylistprice) as avgbuylistprice
  from v_user_cards b
	join mv_prices c on (b.uuid = c.uuid)
    group by b.useremail, b.setcode, b.setname, b.releasedate, b.type, b.color, c.pricedate
union all
select 'all' as grouptype, b.setcode, b.setname, b.releasedate, b.type, b.color,
	c.pricedate, sum(c.avgretailprice) as avgretailprice, sum(c.avgbuylistprice) as avgbuylistprice
  from v_set_cards b
  join mv_prices c on (b.uuid = c.uuid)
    group by b.setcode, b.setname, b.releasedate, b.type, b.color, c.pricedate;
	
create materialized view mv_prices as
select uuid, pricedate, sum(avgretailprice) as avgretailprice, 
sum(avgbuylistprice) as avgbuylistprice from
(select uuid, pricedate, round(avgprice, 2) as avgretailprice, 0 as avgbuylistprice
from mtg_avg_prices where pricetype = 'retail'
union all
select uuid, pricedate, 0 as avgretailprice, round(avgprice, 2) as avgbuylistprice
from mtg_avg_prices where pricetype = 'buylist') as prices
group by uuid, pricedate
order by uuid, pricedate;

create index on mv_prices(uuid);
	
create materialized view mv_price_history as
select b.useremail as grouptype, b.setcode, b.setname, b.releasedate, b.type as cardtype, b.color,
	c.pricedate, sum(c.avgretailprice) as avgretailprice, sum(c.avgbuylistprice) as avgbuylistprice
  from v_user_cards b
	join mv_prices c on (b.uuid = c.uuid)
    group by b.useremail, b.setcode, b.setname, b.releasedate, b.type, b.color, c.pricedate
union all
select 'all' as grouptype, b.setcode, b.setname, b.releasedate, b.type, b.color,
	c.pricedate, sum(c.avgretailprice) as avgretailprice, sum(c.avgbuylistprice) as avgbuylistprice
  from v_set_cards b
  join mv_prices c on (b.uuid = c.uuid)
    group by b.setcode, b.setname, b.releasedate, b.type, b.color, c.pricedate
order by grouptype, setcode;
	


	select grouptype, b.setname, pricedate, sum(avgretailprice) as avgretailprice,
	sum(avgbuylistprice) as avgbuylistprice from mv_price_history b
	join v_user_sets c on (b.setcode = c.setcode)
	where b.grouptype in ('stacy@email.com', 'all')
	and c.useremail = 'stacy@email.com'
	group by grouptype, b.setname, pricedate
	order by grouptype, pricedate;