-- data for cards by set page
create or replace view v_cards_by_set as
select b.useremail as grouptype, b.setcode, b.setname, b.releasedate,
	count(b.uuid) as numcards, sum(b.avgretailprice) as avgretailprice,
	round(cast(count(b.uuid) as decimal(7,2)) / c.totalsetsize, 2) as percentowned
  from v_user_cards b
	join mtg_sets c on (b.setcode = c.code)
    group by b.useremail, b.setcode, b.setname, b.releasedate, c.totalsetsize
union all
select 'all' as grouptype, b.setcode, b.setname, b.releasedate,
	b.totalsetsize as numcards, sum(b.avgretailprice) as avgretailprice,
	0 as percentowned
  from v_set_cards b
    group by b.setcode, b.setname, b.releasedate, b.totalsetsize;
	
create or replace view v_user_sets as
select distinct useremail, setcode, setname from v_user_cards;

	
	select b.* from v_cards_by_set b
	join v_user_sets c on (b.setcode = c.setcode)
	where b.grouptype in ('stacy@email.com', 'all')
	and c.useremail = 'stacy@email.com';